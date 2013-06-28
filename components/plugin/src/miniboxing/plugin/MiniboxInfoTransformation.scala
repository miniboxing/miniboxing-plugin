package miniboxing.plugin

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.InfoTransform
import scala.collection.mutable.HashMap
import scala.tools.nsc.typechecker.Analyzer

trait MiniboxInfoTransformation extends InfoTransform {
  self: MiniboxPhase with MiniboxLogic with MiniboxLogging with MiniboxSpecializationInfo =>

  import global._
  import Flags._
  import definitions._

  /** Type transformation. It is applied to all symbols, compiled or loaded.
   *  If it is a 'no-specialization' run, it is applied only to loaded symbols. */
  override def transformInfo(sym: Symbol, tpe: Type): Type = {
    tpe.resultType match {
      case cinfo @ ClassInfoType(parents, decls, origin) =>
        val tparams  = tpe.typeParams
        if (tparams.isEmpty)
          afterMinibox(parents map (_.typeSymbol.info))
        specialize(origin, cinfo, typeEnv.getOrElse(origin, MiniboxingTypeEnv(Map.empty, Map.empty)))
      case _ =>
        tpe
    }
  }


  class SpecializeTypeMap(current: Symbol) extends TypeMap {
    def extractPSpec(tref: TypeRef) = PartialSpec.fromType(tref)
    override def apply(tp: Type): Type = tp match {
      case tref@TypeRef(pre, sym, args) if args.nonEmpty =>
        val pre1 = this(pre)
        afterMinibox(sym.info)
        specializedClasses(sym).get(extractPSpec(tref)) match {
          case Some(sym1) =>
            val localTParamMap = (sym1.typeParams zip args.map(_.typeSymbol)).toMap
            deferredTypeTags(current) ++= deferredTypeTags(sym1).mapValues(localTParamMap)
            typeRef(pre1, sym1, args)
          case None       => typeRef(pre1, sym, args)
        }
      case _ => tp
    }
  }

  def specializeParentsTypeMapForGeneric(current: Symbol) = new SpecializeTypeMap(current)

  def specializeParentsTypeMapForSpec(spec: Symbol, origin: Symbol, pspec: PartialSpec) = new SpecializeTypeMap(spec) {
    override def extractPSpec(tref: TypeRef) = PartialSpec.fromTypeInContext(tref, pspec)
    override def apply(tp: Type): Type = tp match {
      case tref@TypeRef(pre, sym, args) if sym == origin =>
        tref
      case _ =>
        super.apply(tp)
    }
  }

  /*
   * Substitute the type parameters with their value as given by the 'env'
   * in the type 'tpe'. The replacement is shallow, as the transformation
   * doesn't go deep into the types:
   *   class C[@minispec T]{
   *     def foo: T = ???
   *     def bar: List[T] = ???
   *   }
   * should produce:
   *   class C_J[T$sp](T_Tag: Byte) extends C[T] {
   *     def foo_L: Long       = ??? // <= notice we return long here
   *     def bar_L: List[T$sp] = ??? // <= notice this is List[T$sp] instead of List[Long]
   *                                 //    if the List class is miniboxed, List[T$sp] will actually be an interface
   *                                 //    and it will be inherited by either List_L or List_J.
   *     def foo: T$sp         = minibox2box(foo_L, T_Tag)
   *     def bar: List[T$sp]   = minibox2box(bar_L, T_Tag)
   *
   *  This can be done in two steps:
   *    1. going from T to T$sp, deep transformation (eg List[T] => List[T$sp])
   *    2. going from T$sp to Long, shallow transformation (eg T$sp => Long, but List[T$sp] stays the same)
   */
  private def miniboxSubst(deepEnv: TypeEnv, shallowEnv: TypeEnv, tpe: Type): (Type, List[(Symbol, Type)]) = {
    // Deep transformation, which redirects T to T$sp
    val (deepKeys, deepValues) = deepEnv.toList.unzip
    val deepSubst = new SubstTypeMap(deepKeys, deepValues)

    // Shallow transformation, which redirects T$sp to Long if T$sp represents a miniboxed value
    val (shallowKeys, shallowValues) = shallowEnv.toList.unzip
    var mboxedParams = List[(Symbol, Type)]()

    val shallowSubst = new SubstTypeMap(shallowKeys, shallowValues) {
      override def mapOver(tp: Type): Type = tp match {
        case TypeRef(pre, sym, args) =>
          shallowEnv.get(sym) match {
            case Some(tpe) if args.isEmpty => tpe
            case _ => tp // we don't want the mapper to go further inside the type
          }
        case MethodType(params, result) =>
          val paramTypes = params.map(_.tpe)
          val params1 = mapOver(params)
          mboxedParams :::= (params1 zip paramTypes).collect({ case (p1, t) if p1.tpe == LongClass.tpe && t != LongClass.tpe => (p1, t) })
          val result1 = this(result)
          if ((params1 eq params) && (result1 eq result)) tp
          else copyMethodType(tp, params1, result1.substSym(params, params1))
        case _ =>
          super.mapOver(tp)
      }
    }

    (shallowSubst(deepSubst(tpe)), mboxedParams)
  }

  /*
   * Every specialized class has its own symbols for the type parameters,
   * this function replaces the ones of the original class with the ones
   * from the specialized class.
   */
  private def substParams(pmap: ParamMap)(tpe: Type): Type = {
    val (oldTParams, newTParams) = pmap.toList.unzip
    tpe.instantiateTypeParams(oldTParams, newTParams map (_.tpe))
  }

  /*
   * This removes fields and constructors from a class while leaving the
   * setters and getters in place. The effect is that the class automatically
   * becomes an interface
   */
  private def removeClassFields(clazz: Symbol, decls1: Scope): Scope = {
    val decls = decls1.cloneScope
    for (mbr <- decls) {
      mbr.setFlag(DEFERRED)
      if ((mbr.isTerm && !mbr.isMethod) || (mbr.isConstructor))
        decls unlink mbr
    }
    // Add trait constructor and set the trait flag
    // decls.enter(clazz.newMethod(nme.MIXIN_CONSTRUCTOR, clazz.pos) setInfo MethodType(Nil, UnitClass.tpe))
    clazz.setFlag(TRAIT)
    // This needs to be delayed until trees have been duplicated, else
    // instantiation will fail, since C becomes an abstract class
    clazz.setFlag(INTERFACE)
    clazz.setFlag(ABSTRACT)
    decls
  }


  def specialize(origin: Symbol, originTpe: ClassInfoType, outerEnv: MiniboxingTypeEnv): Type = {

    def specialize1(pspec: PartialSpec, decls: Scope): Symbol = {

      val specParamValues = typeParamValues(origin, pspec)
      val specName = specializedName(origin.name, specParamValues).toTypeName
      val bytecodeClass = origin.owner.info.decl(specName)
      bytecodeClass.info // TODO: we have 5054 here, but even this doesn't work
      val spec = origin.owner.newClass(specName, origin.pos, origin.flags)

      spec.sourceFile = origin.sourceFile
      currentRun.symSource(spec) = origin.sourceFile
      baseClass(spec) = origin

      val pmap = ParamMap(origin.typeParams, spec)
      typeParamMap(spec) = pmap.map(_.swap).toMap

      // T -> Long
      val implEnv: TypeEnv = pspec flatMap {
        case (p, Boxed)     => None // stays the same
        case (p, Miniboxed) => Some((pmap(p), LongClass.tpe))
      }
      // T -> Tsp
      val ifaceEnv: TypeEnv = pmap mapValues (_.tpe)

      // Insert the newly created symbol in our various maps that are used by
      // the tree transformer.
      specializedClasses(origin) += pspec -> spec
      typeEnv(spec) = MiniboxingTypeEnv(deepEnv = ifaceEnv, shallowEnv = implEnv)
      partialSpec(spec) = pspec

      // declarations inside the specialized class - to be filled in later
      val specScope = newScope
      deferredTypeTags(spec) = HashMap()

      // create the type of the new class
      val localPspec: PartialSpec = pspec.map({ case (t, sp) => (pmap(t), sp)}) // Tsp -> Boxed/Miniboxed
      val specializeParents = specializeParentsTypeMapForSpec(spec, origin, localPspec)
      val specializedInfoType: Type = {
        val sParents = (origin.info.parents ::: List(origin.tpe)) map {
          t => (miniboxSubst(ifaceEnv, EmptyTypeEnv, t)._1)
        } map specializeParents

        // parameters which are not fixed
        val newTParams: List[Symbol] = origin.typeParams.map(pmap)
        GenPolyType(newTParams, ClassInfoType(sParents, specScope, spec))
      }
      afterMinibox(spec setInfo specializedInfoType)

      // Add type tag fields for each parameter. Will be copied in specialized subclasses.
      val typeTagMap: List[(Symbol, Symbol)] =
        (for (tparam <- origin.typeParams if tparam.hasFlag(MINIBOXED) && pspec(tparam) == Miniboxed) yield {
          val sym =
            if (origin.isTrait)
              spec.newMethodSymbol(typeTagName(tparam), spec.pos, DEFERRED).setInfo(MethodType(List(), ByteTpe))
            else
              spec.newValue(typeTagName(tparam), spec.pos, SYNTHETIC | PARAMACCESSOR | PrivateLocal).setInfo(ByteTpe)

          sym setFlag MINIBOXED
          if (origin.isTrait) {
            deferredTypeTags(spec) += sym -> pmap(tparam)
            memberSpecializationInfo(sym) = DeferredTypeTag(tparam)
          }

          specScope enter sym
          (pmap(tparam), sym)
        })

      // Record the new mapping for type tags to the fields carrying them
      globalTypeTags(spec) = typeTagMap.toMap

      // adding the type tags as constructor arguments
      for (ctor <- decls.filter(sym => sym.name == nme.CONSTRUCTOR)) {
        val newCtor = ctor.cloneSymbol(spec)
        newCtor setFlag MINIBOXED
        newCtor modifyInfo { info =>
          val info0 = info.asSeenFrom(spec.tpe, ctor.owner)
          val info1 = info0.substThis(origin, spec) // Is this still necessary?
          val (info2, mboxedArgs) = miniboxSubst(ifaceEnv, implEnv, info1)
          val tagParams = typeTagMap map (_._2.cloneSymbol(newCtor, SYNTHETIC))
          localTypeTags(newCtor) = typeTagMap.map(_._1).zip(tagParams).toMap
          def transformArgs(tpe: Type): Type = tpe match {
            case MethodType(params, ret) =>
              MethodType(tpe.params, transformArgs(ret))
            case TypeRef(_, _, _) =>
              spec.tpe
            case _ =>
              tpe
          }
          miniboxedArgs(newCtor) = mboxedArgs
          overloads.get(ctor) match {
            case Some(map) => map += pspec -> newCtor
            case None => overloads(ctor) = HashMap(pspec -> newCtor)
          }
          MethodType(tagParams.toList, transformArgs(info2))
        }
        memberSpecializationInfo(newCtor) = SpecializedImplementationOf(ctor)
        specScope enter newCtor
      }

      // Copy the members of the original class to the specialized class.
      val newMembers: Map[Symbol, Symbol] =
        (for (mbr <- decls.toList if mbr.owner == origin && !mbr.isConstructor) yield {
          val newMbr = mbr.cloneSymbol(spec)
          if (mbr.isMethod) {
            if (base(mbr) == mbr)
              base(newMbr) = newMbr
            else
              base(newMbr) = mbr
          }
          (mbr, newMbr)
        }).toMap


      // Replace the info in the copied members to reflect their new class
      for ((m, newMbr) <- newMembers if !m.isConstructor) {

        newMbr setFlag MINIBOXED
        newMbr modifyInfo { info =>

          val info0 = info.asSeenFrom(spec.tpe, m.owner)
          val info1 = info0.substThis(origin, spec)
          val info2 =
            if (m.isTerm && !m.isMethod)
              // TODO: We assume we access these values via accessors anyway
              miniboxSubst(ifaceEnv, implEnv, info1)._1
            else
              info1

          val paramUpdate = m.info.paramss.flatten.zip(info2.paramss.flatten).toMap
          val oldParams = miniboxedArgs.getOrElse(m, Nil)

          miniboxedArgs(newMbr) = oldParams.map({ case (s, t) => (paramUpdate(s), pmap(t.typeSymbol).tpe)})

          info2
        }

        localTypeTags(newMbr) = localTypeTags.getOrElse(m, Map.empty).map(p => pmap(p._1)).zip(newMbr.info.params).toMap
        debug(spec + " entering: " + newMbr)
        specScope enter newMbr
      }

      // Record how the body of these members should be generated
      for ((mbr, newMbr) <- newMembers) {
        if (mbr.isConstructor || (mbr.isTerm && !mbr.isMethod)) {
          memberSpecializationInfo(newMbr) = SpecializedImplementationOf(mbr)
        } else {
          if (mbr.isDeferred)
            memberSpecializationInfo(newMbr) = Interface()

          // Check whether the method is the one that will carry the
          // implementation. If yes, find the original method from the original
          // class from which to copy the implementation. If no, find the method
          // that will have an implementation and forward to it.
          if (overloads(mbr)(pspec) == mbr) {
            if (mbr.hasAccessorFlag) {
              memberSpecializationInfo(newMbr) = memberSpecializationInfo.get(mbr) match {
                case Some(ForwardTo(_, original, _, _)) =>
                  FieldAccessor(newMembers(original.accessed))
                case _ =>
                  global.error("Unaccounted case: " + memberSpecializationInfo.get(mbr)); ???
              }
            } else {
              memberSpecializationInfo.get(mbr) match {
                case Some(ForwardTo(_, original, _, _)) =>
                  memberSpecializationInfo(newMbr) = SpecializedImplementationOf(original)
                case Some(x) =>
                  global.error("Unaccounted case: " + x)
                case None =>
                  memberSpecializationInfo(newMbr) = SpecializedImplementationOf(mbr)
              }
            }
          } else {
            val target = newMembers(overloads(mbr)(pspec))
            val wrapTagMap = localTypeTags.getOrElse(newMbr, Map.empty).map{ case (ttype, ttag) => (pmap.getOrElse(ttype, ttype), ttag) } ++ globalTypeTags(spec)
            val targTagMap = localTypeTags.getOrElse(target, Map.empty)
            memberSpecializationInfo(newMbr) = genForwardingInfo(newMbr, wrapTagMap, target, targTagMap)
          }
        }
      }

      // populate the overloads data structure for the new members also
      for ((m, newMbr) <- newMembers if (m.isMethod && !m.isConstructor)) {
        val newMbrMeantForSpec = newMembers(overloads(m)(pspec))
        if (!(overloads isDefinedAt newMbrMeantForSpec)) {
          overloads(newMbrMeantForSpec) = new HashMap[PartialSpec, Symbol]
          for ((s, m) <- overloads(m)) {
            overloads(newMbrMeantForSpec)(s) = newMembers(m)
          }
        }
        overloads(newMbr) = overloads(newMbrMeantForSpec)
      }

      // deferred type tags:
      addDeferredTypeTagImpls(spec, specScope)

      spec
    }

    def widen(specs: List[PartialSpec]): List[Symbol] = {

      baseClass(origin) = origin
      typeParamMap(origin) = origin.info.typeParams.map((p: Symbol) => (p, p)).toMap
      deferredTypeTags(origin) = HashMap()
      specializedClasses(origin) = HashMap()

      // we only specialize the members that are defined in the current class
      val members = origin.info.members.filter(_.owner == origin).toList

      // TODO: Do we actually want to special-case constructors?
      val methods = members.filter(s => s.isMethod && !(s.isConstructor || s.isGetter || s.isSetter))
      val getters = members.filter(_.isGetter)
      val setters = members.filter(_.isSetter)
      val fields = members.filter(m => m.isTerm && !m.isMethod)

      var newMembers = List[Symbol]()

      // we make specialized overloads for every member of the original class
      for (member <- methods ::: getters ::: setters if (needsSpecialization(origin, member))) {
        val overloadsOfMember = new HashMap[PartialSpec, Symbol]
        for (spec <- specs) {
          var newMbr = member
          if (!isAllAnyRef(spec)) {
            val env: TypeEnv = spec map {
              case (p, v) => (p, if (v == Boxed) p.tpe else LongClass.tpe)
            }

            newMbr = member.cloneSymbol(origin)

            newMbr setFlag MINIBOXED
            newMbr setName (specializedName(member.name, typeParamValues(origin, spec)))
            newMbr modifyInfo (info => {
              val (info0, mbArgs) = miniboxSubst(EmptyTypeEnv, env, info.asSeenFrom(newMbr.owner.thisType, newMbr.owner))
              val localTags =
                for (tparam <- origin.typeParams if tparam.hasFlag(MINIBOXED) && spec(tparam) == Miniboxed)
                  yield (tparam, newMbr.newValue(typeTagName(tparam), newMbr.pos).setInfo(ByteClass.tpe))
              localTypeTags(newMbr) = localTags.toMap
              val tagParams = localTags.map(_._2)
              val info1 =
                info0 match {
                  case mt: MethodType =>
                    MethodType(tagParams, mt)
                  case nmt: NullaryMethodType =>
                    MethodType(tagParams, nmt.resultType)
                }
              miniboxedArgs(newMbr) = mbArgs
              info1
            })
            newMembers ::= newMbr
          } else {
            miniboxedArgs(newMbr) = Nil
          }

          overloadsOfMember(spec) = newMbr
          overloads(newMbr) = overloadsOfMember
          base(newMbr) = member
        }

        for (spec <- specs; newMbr <- overloadsOfMember get spec)
          memberSpecializationInfo(newMbr) = genForwardingInfo(newMbr, localTypeTags.getOrElse(newMbr, Map.empty), member, Map.empty)
      }

      origin.info.decls.toList ++ newMembers
    }

    // add members derived from deferred type tags
    def addDeferredTypeTagImpls(origin: Symbol, scope: Scope) =
      if (!origin.isTrait) {
        val deferredTags = deferredTypeTags(origin)
        // classes satisfy the deferred tags immediately, no need to keep them
        for ((method, tparam) <- deferredTags) {
          val impl = method.cloneSymbol(origin).setFlag(MINIBOXED)
          memberSpecializationInfo(impl) = DeferredTypeTagImplementation(tparam)
          scope enter impl
        }
        deferredTypeTags(origin).clear()
      }

    // begin specialize
    val specs = if (isSpecializableClass(origin)) specializations(origin.info.typeParams) else Nil
    specs.map(_.map(_._1.setFlag(MINIBOXED)))

    if (specs.nonEmpty) {
      log("Specializing " + origin + "...\n")

      // step1: widen the class or trait
      val scope1 = newScopeWith(widen(specs): _*)

      // mark this symbol as the base of a miniboxed hierarchy
      specializedBase += origin

      // step2: create subclasses
      val classes = specs map {
        env =>
        val spc      = specialize1(env, scope1)
        val existing = origin.owner.info.decl(spc.name)

        // a symbol for the specialized class already exists if there's a classfile for it.
        // keeping both crashes the compiler on test/files/pos/spec-Function1.scala
        if (existing != NoSymbol)
          origin.owner.info.decls.unlink(existing)

        // TODO: overrides in the specialized class
        afterMinibox(origin.owner.info.decls enter spc)

        spc
      }

      // for traits resulting from classes inheriting each other we need to insert an artificial AnyRef parent
      val artificialAnyRefReq = !origin.isTrait && ((originTpe.parents.size >= 1) && (specializedBase(originTpe.parents.head.typeSymbol)))
      val artificialAnyRef = if (artificialAnyRefReq) List(AnyRefTpe) else Nil
      val parents1 = artificialAnyRef ::: originTpe.parents

      val scope2 = removeClassFields(origin, scope1)
      log("  // interface:")
      log("  " + origin.defString + " {")
      for (decl <- scope2.toList.sortBy(_.defString))
        log(f"    ${decl.defString}%-70s")

      log("  }\n")

      classes foreach { cls =>
      log("  // specialized class:")
      log("  " + cls.defString + " {")
      for (decl <- cls.info.decls.toList.sortBy(_.defString))
        log(f"    ${decl.defString}%-70s // ${memberSpecializationInfo.get(decl).map(_.toString).getOrElse("no info")}")
        log("  }\n")
      }
      log("\n\n")
      origin.resetFlag(FINAL)

      GenPolyType(origin.info.typeParams, ClassInfoType(parents1, scope2, origin))
    } else {
      // TODO: overrides in the specialized class
      deferredTypeTags(origin) = HashMap()
      val scope1 = newScopeWith(originTpe.decls.toList /* ++ specialOverrides(origin) */: _*)
      val specializeTypeMap = specializeParentsTypeMapForGeneric(origin)
      val parents1 = originTpe.parents map specializeTypeMap
      addDeferredTypeTagImpls(origin, scope1)
      GenPolyType(origin.info.typeParams, ClassInfoType(parents1, scope1, origin))
    }
  }

  /**
   * Generate the information about how arguments and return value should
   * be converted when forwarding to `target`.
   */
  private def genForwardingInfo(wrapper: Symbol, wrapperTags: Map[Symbol, Symbol], target: Symbol, targetTags: Map[Symbol, Symbol]): ForwardTo = {
    def genCastInfo(srcType: Type, tgtType: Type): CastInfo = {
      val srcTypeSymbol: Symbol = srcType.typeSymbol
      val tgtTypeSymbol: Symbol = tgtType.typeSymbol

      if (srcTypeSymbol == LongClass && tgtTypeSymbol != LongClass) {
        CastMiniboxToBox(wrapperTags(tgtTypeSymbol))
      } else if (srcTypeSymbol != LongClass && tgtTypeSymbol == LongClass) {
        CastBoxToMinibox(wrapperTags(srcTypeSymbol))
      } else if (srcTypeSymbol == tgtTypeSymbol) {
        if (srcType == tgtType) NoCast
        else {
          // We have something like Foo[T] vs Foo[Long] which will be the same
          // after erasure. For now, just pretend that they are the same.
          // - we should not have this case anymore.
          ??? // AsInstanceOfCast
        }
        // Arrays are 'erased' to Any
      } else if (srcTypeSymbol == ArrayClass && tgtTypeSymbol == AnyClass) {
        NoCast
      } else if (srcTypeSymbol == AnyClass && tgtTypeSymbol == ArrayClass) {
        AsInstanceOfCast
      } else {
        log(wrapper + ": " + wrapper.tpe + " ==> " + params(wrapper))
        log(target + ": " + target.tpe + " ==> " + params(target))
        log(srcTypeSymbol)
        log(tgtTypeSymbol)
        log("A cast which is neither boxing, nor unboxing when handling `ForwardTo`.")
        log(srcTypeSymbol + " --> " + tgtTypeSymbol)
        ???
      }
    }

    def params(target: Symbol): (List[Symbol], List[Symbol]) =
      if (!target.isMethod || (!target.name.toString.contains("_J") && !target.name.toString.contains("_L")))
        (Nil, target.info.params)
      else
        target.info match {
          case MethodType(typetags, result) =>
            (typetags, result.params)
          case nmt: NullaryMethodType =>
            (Nil, Nil)
        }

    def typeParams = {
      val targetTParams = targetTags.map(_.swap).toMap
      val targs = params(target)._1.map(targetTParams)
      val tagParams = targs.map(wrapperTags)
      tagParams
    }

    // TODO: only basic parameters should go here
    val wrapParams = params(wrapper)._2.map(_.tpe)
    val targParams = params(target)._2.map(_.tpe)
    assert(wrapParams.length == targParams.length, (wrapParams, targParams))
    val paramCasts = (wrapParams zip targParams) map {
      case (wtp, ttp) => genCastInfo(wtp, ttp)
    }
    val retCast = genCastInfo(target.tpe.finalResultType, wrapper.tpe.finalResultType)

    // TODO: add type tag forwarding info
    ForwardTo(typeParams, target, retCast, paramCasts)
  }
}
