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

  var makeTrait = false

  /** Type transformation. It is applied to all symbols, compiled or loaded.
   *  If it is a 'no-specialization' run, it is applied only to loaded symbols. */
  override def transformInfo(sym: Symbol, tpe: Type): Type = {
    tpe.resultType match {
      case cinfo @ ClassInfoType(parents, decls, clazz) =>
        val tparams  = tpe.typeParams
        if (tparams.isEmpty)
          afterMinibox(parents map (_.typeSymbol.info))

        val parents1 = parents mapConserve specializedType
        val newScope = newScopeWith(specializeClass(clazz, typeEnv.getOrElse(clazz, MiniboxingTypeEnv(Map.empty, Map.empty))) /* ++ specialOverrides(clazz) */: _*)
        // If tparams.isEmpty, this is just the ClassInfoType.
        GenPolyType(tparams, ClassInfoType(parents1, newScope, clazz))
      case _ =>
        tpe
    }
  }

  lazy val specializedType: TypeMap =
    new TypeMap {
      override def apply(tp: Type): Type = tp match {
        case tref@TypeRef(pre, sym, args) if args.nonEmpty =>
          val pre1 = this(pre)
          // when searching for a specialized class, take care to map all
          // type parameters that are subtypes of AnyRef to AnyRef
          specializedClasses(sym).get(PartialSpec.fromType(tref)) match {
            case Some(sym1) => typeRef(pre1, sym1, args)
            case None       => typeRef(pre1, sym, args)
          }
        case _ => tp
      }
    }

  def makeTraits() {
    for (clazz <- specializedBase) {
      clazz.setFlag(INTERFACE)
      clazz.setFlag(ABSTRACT)
    }
    makeTrait = true
  }

  /**
   * For each method in the original class and each partial specialization
   * we generate another methods, which we call "specialized overload" that
   * works with values according to the representation prescribed by the
   * partial specialization.
   *
   * Now the original class becomes a template for creating the interface and the
   * specialized versions.
   */
  def widenClass(clazz: Symbol, specs: List[PartialSpec]) = {

    baseClass(clazz) = clazz
    typeParamMap(clazz) = clazz.info.typeParams.map((p: Symbol) => (p, p)).toMap

    // we only specialize the members that are defined in the current class
    val members = clazz.info.members.filter(_.owner == clazz).toList

    // TODO: Do we actually want to special-case constructors?
    val methods = members.filter(s => s.isMethod && !(s.isConstructor || s.isGetter || s.isSetter))
    val getters = members.filter(_.isGetter)
    val setters = members.filter(_.isSetter)
    val fields = members.filter(m => m.isTerm && !m.isMethod)

    // we make specialized overloads for every member of the original class
    for (member <- methods ::: getters ::: setters if (needsSpecialization(clazz, member))) {
      val overloadsOfMember = new HashMap[PartialSpec, Symbol]
      for (spec <- specs) {
        var newMbr = member
        if (!isAllAnyRef(spec)) {
          val env: TypeEnv = spec map {
            case (p, v) => (p, if (v == Boxed) p.tpe else LongClass.tpe)
          }

          newMbr = member.cloneSymbol(clazz)

          newMbr setFlag MINIBOXED
          newMbr setName (specializedName(member.name, typeParamValues(clazz, spec)))
          newMbr modifyInfo (info => {
            val (info0, mbArgs) = miniboxSubst(EmptyTypeEnv, env, info.asSeenFrom(newMbr.owner.thisType, newMbr.owner))
            val localTags =
              for (tparam <- clazz.typeParams if tparam.hasFlag(MINIBOXED) && spec(tparam) == Miniboxed)
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
          clazz.info.resultType.decls enter (newMbr)
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
  private def removeClassFields(clazz: Symbol) = {
    val decls = clazz.info.decls //.cloneScope
    for (mbr <- decls) {
      mbr.setFlag(DEFERRED)
      if ((mbr.isTerm && !mbr.isMethod) || (mbr.isConstructor))
        decls unlink mbr
    }
    // Add trait constructor and set the trait flag
    clazz.info.decls.enter(clazz.newMethod(nme.MIXIN_CONSTRUCTOR, clazz.pos) setInfo MethodType(Nil, UnitClass.tpe))
    clazz.setFlag(TRAIT)
    // This needs to be delayed until trees have been duplicated, else
    // it will create pretty ugly problems since the new C is not a valid
    // instantiation, since C becomes an abstract class
    if (makeTrait) {
      clazz.setFlag(INTERFACE)
      clazz.setFlag(ABSTRACT)
    }
  }


  def specializeClass(clazz: Symbol, outerEnv: MiniboxingTypeEnv): List[Symbol] = {

    // TODO: Decide what we need: PartialSpec or MiniboxingTypeEnv?
    def specializeClass1(spec: PartialSpec): Symbol = {
      val sParamValues = typeParamValues(clazz, spec)
      val sClassName = specializedName(clazz.name, sParamValues).toTypeName
      val bytecodeClass = clazz.owner.info.decl(sClassName)
      bytecodeClass.info // TODO: we have 5054 here, but even this doesn't work
      val sClass = clazz.owner.newClass(sClassName, clazz.pos, clazz.flags)

      sClass.sourceFile = clazz.sourceFile
      currentRun.symSource(sClass) = clazz.sourceFile
      baseClass(sClass) = clazz

      // `pmap` is a map from the parameters of the original class to those of the current specialized version.
      val pmap = ParamMap(clazz.typeParams, sClass)
      typeParamMap(sClass) = pmap.map(_.swap).toMap

      // When copying information from the original class, we need to change
      // types. For the fields, we need to convert them to the new
      // representation (implEnv). For the methods, since we already have
      // one overload for each representation, we only need to change the
      // type parameter symbols to the fresh ones (ifaceEnv).
      val implEnv: TypeEnv = spec flatMap {
        case (p, Boxed)     => None // stays the same
        case (p, Miniboxed) => Some((pmap(p), LongClass.tpe))
      }
      val ifaceEnv: TypeEnv = pmap mapValues (_.tpe)

      // Insert the newly created symbol in our various maps that are used by
      // the tree transformer.
      specializedClasses.get(clazz) match {
        case Some(map) => map += spec -> sClass
        case None => specializedClasses(clazz) = collection.mutable.HashMap(spec -> sClass)
      }
      typeEnv(sClass) = MiniboxingTypeEnv(deepEnv = ifaceEnv, shallowEnv = implEnv)
      partialSpec(sClass) = spec

      // declarations inside the specialized class - to be filled in later
      val sClassDecls = newScope

      // create the type of the new class
      // TODO!
      val specializedInfoType: Type = {
        assert(clazz.info.parents == List(AnyRefClass.tpe) || clazz.info.parents == List(ObjectClass.tpe), "TODO: Here we should also perform parent rewiring: D_L extends C_L, not simply C: parents: " + clazz + " parents: " + clazz.info.parents)
        val sParents = (clazz.info.parents ::: List(clazz.tpe)) map {
          // TODO: This probably won't work, as we have to take the parent's miniboxed fields into account
          t => (miniboxSubst(ifaceEnv, EmptyTypeEnv, t)._1)
        }

        // parameters which are not fixed
        val newTParams: List[Symbol] = clazz.typeParams.map(pmap)
        GenPolyType(newTParams, ClassInfoType(sParents, sClassDecls, sClass))
      }
      afterMinibox(sClass setInfo specializedInfoType)

      // Add type tag fields for each parameter. Will be copied in specialized
      // subclasses.
      val typeTagMap: List[(Symbol, Symbol)] =
        (for (tparam <- clazz.typeParams if tparam.hasFlag(MINIBOXED) && spec(tparam) == Miniboxed) yield {
          val sym = sClass.newValue(typeTagName(tparam), sClass.pos, SYNTHETIC | PARAMACCESSOR | PrivateLocal)
          sym setInfo ByteClass.tpe
          sym setFlag MINIBOXED

          sClassDecls enter sym
          (pmap(tparam), sym)
        })
      // Record the new mapping for type tags to the fields carrying them
      globalTypeTags(sClass) = typeTagMap.toMap

      // adding the type tags as constructor arguments
      for (ctor <- clazz.info.members.filter(sym => sym.owner == clazz && sym.isConstructor)) {
        val newCtor = ctor.cloneSymbol(sClass)
        newCtor setFlag MINIBOXED
        newCtor modifyInfo { info =>
          val info0 = info.asSeenFrom(sClass.tpe, ctor.owner)
          val info1 = info0.substThis(clazz, sClass) // Is this still necessary?
          val (info2, mboxedArgs) = miniboxSubst(ifaceEnv, implEnv, info1)
          val tagParams = typeTagMap map (_._2.cloneSymbol(newCtor, SYNTHETIC))
          localTypeTags(newCtor) = typeTagMap.map(_._1).zip(tagParams).toMap
          def transformArgs(tpe: Type): Type = tpe match {
            case MethodType(params, ret) =>
              MethodType(tpe.params, transformArgs(ret))
            case TypeRef(_, _, _) =>
              sClass.tpe
            case _ =>
              tpe
          }
          miniboxedArgs(newCtor) = mboxedArgs
          overloads.get(ctor) match {
            case Some(map) => map += spec -> newCtor
            case None => overloads(ctor) = collection.mutable.HashMap(spec -> newCtor)
          }
          MethodType(tagParams.toList, transformArgs(info2))
        }
        memberSpecializationInfo(newCtor) = SpecializedImplementationOf(ctor)
        sClassDecls enter newCtor
      }

      // Copy the members of the original class to the specialized class.
      val newMembers: Map[Symbol, Symbol] =
        (for (mbr <- clazz.info.members if mbr.owner == clazz && !mbr.isConstructor) yield {
          val newMbr = mbr.cloneSymbol(sClass)
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

          val info0 = info.asSeenFrom(sClass.tpe, m.owner)
          val info1 = info0.substThis(clazz, sClass)
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
        debug(sClass + " entering: " + newMbr)
        sClassDecls enter newMbr
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
          if (overloads(mbr)(spec) == mbr) {
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
            val target = newMembers(overloads(mbr)(spec))
            val wrapTagMap = localTypeTags.getOrElse(newMbr, Map.empty).map{ case (ttype, ttag) => (pmap.getOrElse(ttype, ttype), ttag) } ++ globalTypeTags(sClass)
            val targTagMap = localTypeTags.getOrElse(target, Map.empty)
            memberSpecializationInfo(newMbr) = genForwardingInfo(newMbr, wrapTagMap, target, targTagMap)
          }
        }
      }

      // populate the overloads data structure for the new members also
      for ((m, newMbr) <- newMembers if (m.isMethod && !m.isConstructor)) {
        val newMbrMeantForSpec = newMembers(overloads(m)(spec))
        if (!(overloads isDefinedAt newMbrMeantForSpec)) {
          overloads(newMbrMeantForSpec) = new HashMap[PartialSpec, Symbol]
          for ((s, m) <- overloads(m)) {
            overloads(newMbrMeantForSpec)(s) = newMembers(m)
          }
        }
        overloads(newMbr) = overloads(newMbrMeantForSpec)
      }

      sClass
    }


    val specs = if (isSpecializableClass(clazz)) specializations(clazz.info.typeParams) else Nil
    specs.map(_.map(_._1.setFlag(MINIBOXED)))

    if (specs.nonEmpty) {
      log("Specializing " + clazz + "...\n")

      // step1: widen the class
      widenClass(clazz, specs)

      // mark this symbol as the base of a miniboxed hierarchy
      specializedBase += clazz

      // step2: create subclasses
      val classes = specs map {
        env =>
        val spc      = specializeClass1(env)
        val existing = clazz.owner.info.decl(spc.name)

        // a symbol for the specialized class already exists if there's a classfile for it.
        // keeping both crashes the compiler on test/files/pos/spec-Function1.scala
        if (existing != NoSymbol)
          clazz.owner.info.decls.unlink(existing)

        afterMinibox(clazz.owner.info.decls enter spc)

        spc
      }

      removeClassFields(clazz)
      log("  // interface:")
      log("  " + clazz.defString + " {")
      for (decl <- clazz.info.decls.toList.sortBy(_.defString))
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

      clazz.resetFlag(FINAL)
    }

    clazz.info.decls.toList
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
          /*
           * We have something like Foo[T] vs Foo[Long] which will be the same
           * after erasure. For now, just pretend that they are the same.
           */
          AsInstanceOfCast
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
