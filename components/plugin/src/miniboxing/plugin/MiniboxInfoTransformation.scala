package miniboxing.plugin

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.InfoTransform
import scala.collection.mutable.HashMap
import scala.tools.nsc.typechecker.Analyzer
import com.sun.org.apache.xerces.internal.dom.DeepNodeListImpl

trait MiniboxInfoTransformation extends InfoTransform {
  self: MiniboxPhase with MiniboxLogic with MiniboxLogging with MiniboxSpecializationInfo =>

  import global._
  import Flags._
  import definitions._

  /**
   * This function receives (among others) the symbol for a parameterized type
   * which we call "the generic class" or "the original class". Based on this
   * class it creates a number of "specialized versions", one for each
   * "partial specialization".
   *
   * The generic class and specialized versions can be used interchangeably
   * but each of them is optimized for a particular representation of the
   * values that it works with. For example, the generic version of `List`
   * is optimized for reference types like `String`, `Symbol`, etc. while
   * the specialized version is optimized for `Int`, `Double`, etc. each
   * of these classes have methods to receive parameters both boxed and
   * `miniboxed`.
   *
   * All these classes extend a "specialized interface" that contains all
   * their methods.
   */
  override def transformInfo(sym: Symbol, tpe: Type): Type =
    /*
     *  TODO: for non specialized classes we need to add `special overrides`
     *  and bridge methods
     */
    if (isSpecializableClass(sym)) {
      log("Specializing " + sym + "...\n")

      // mark this symbol as the base of a miniboxed hierarchy
      specializedBase += sym

      tpe match {
        case PolyType(tArgs, ClassInfoType(parents, decls, typeSym)) =>
          val clazz = sym
          // Create a list of partial specializations
          val envs = specializations(tpe.typeParams)

          // For each method in the original class, add its specialized overloads
          widenClass(clazz, envs)

//          log("-------------- ORIGINAL CLASS ----------------")
//          log(clazz.defString)
//          for (decl <- clazz.info.decls.toList.sortBy(_.nameString))
//            log(f"  ${decl.defString}%-70s => ${memberSpecializationInfo.get(decl)}")

          // Create the actual specialized classes
          val classes = envs map (specializeClass(clazz, _))

          // Now we remove the fields from the class and leave the getters and setters as abstract
          removeClassFields(clazz)

          log("  // interface:")
          log("  " + clazz.defString + " {")
          for (decl <- clazz.info.decls.toList.sortBy(_.nameString))
            log(f"    ${decl.defString}%-70s")
          log("  }\n")

          classes foreach { cls =>
            log("  // specialized class:")
            log("  " + cls.defString + " {")
            for (decl <- cls.info.decls.toList.sortBy(_.nameString))
              log(f"    ${decl.defString}%-70s // ${memberSpecializationInfo.get(decl).map(_.toString).getOrElse("no info")}")
            log("  }\n")
          }
          log("\n\n")

//          log("-------------- TEMPLATE MEMBERS ----------------")
//          for (decl <- templateMembers.toList.sortBy(_.nameString))
//            log(f"  ${decl.defString}%-70s => ${memberSpecializationInfo.get(decl)}")

          tpe
        case _ =>
          log("Not specializing: " + sym)
          tpe
      }
    } else tpe

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
              for (tparam <- clazz.typeParams if spec(tparam) == Miniboxed)
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
    clazz.setFlag(INTERFACE)
    clazz.setFlag(ABSTRACT)
  }

  /**
   * Specialize class `clazz`. `spec` gives the representation for the type parameters.
   */
  def specializeClass(clazz: Symbol, spec: PartialSpec): Symbol = {
    val sParamValues = typeParamValues(clazz, spec)
    val sClassName = specializedName(clazz.name, sParamValues).toTypeName
    val sClass = clazz.owner.newClass(sClassName, clazz.pos, clazz.flags)

    sClass.sourceFile = clazz.sourceFile
    currentRun.symSource(sClass) = clazz.sourceFile
    baseClass(sClass) = clazz

    /*
     * `pmap` is a map from the parameters of the original class to those
     * of the current specialized version.
     */
    val pmap = ParamMap(clazz.typeParams, sClass)
    typeParamMap(sClass) = pmap.map(_.swap).toMap

    /*
     * When copying information from the original class, we need to change
     * types. For the fields, we need to convert them to the new
     * representation (implEnv). For the methods, since we already have
     * one overload for each representation, we only need to change the
     * type parameter symbols to the fresh ones (ifaceEnv).
     */
    val implEnv: TypeEnv = spec flatMap {
      case (p, Boxed)     => None // stays the same
      case (p, Miniboxed) => Some((pmap(p), LongClass.tpe))
    }
    val ifaceEnv: TypeEnv = pmap mapValues (_.tpe)

    /*
     * Insert the newly created symbol in our various maps that are used by
     * the tree transformer.
     */
    specializedClasses.get(clazz) match {
      case Some(map) => map += spec -> sClass
      case None => specializedClasses(clazz) = collection.mutable.HashMap(spec -> sClass)
    }
    typeEnv(sClass) = MiniboxingTypeEnv(deepEnv = ifaceEnv, shallowEnv = implEnv)
    partialSpec(sClass) = spec //.map({ case (t, enc) => (pmap(t), enc)})

    // declarations inside the specialized class - to be filled in later
    val sClassDecls = newScope

    // create the type of the new class
    val specializedInfoType: Type = {
      /*
       * for:
       *   class C[@minispec T]
       *   class D[@minispec T] extends C[T]
       *
       *      C
       *    / | \
       * C_L  |  C_J
       *  |   |   |
       *  |   D   |
       *  | /   \ |
       * D_L     D_J
       *
       * we should have:
       *   trait C[@minispec T]
       *   trait D[@minispec T] extends C[T]
       *   ...
       *   class D_L[T$sp] extends C_L[T$sp] with D[T$sp] // <= first the parents, adapted and then the interface
       *                                                  // thankfully there's no type checking at this point as
       *                                                  // we're creating types that are not valid (since a C and D
       *                                                  // are still classes)
       */
      assert(clazz.info.parents == List(AnyRefClass.tpe), "TODO: Here we should also perform parent rewiring: D_L extends C_L, not simply C: parents: " + clazz.info.parents)
      val sParents = (clazz.info.parents ::: List(clazz.tpe)) map {
        // TODO: This probably won't work, as we have to take the parent's miniboxed fields into account
        t => (miniboxSubst(ifaceEnv, EmptyTypeEnv, t)._1)
      }

      // parameters which are not fixed
      val newTParams: List[Symbol] = clazz.typeParams.map(pmap)
      GenPolyType(newTParams, ClassInfoType(sParents, sClassDecls, sClass))
    }
    sClass setInfo specializedInfoType

    /*
     * Add type tag fields for each parameter. Will be copied in specialized
     * subclasses.
     *
     * NOTE: We need type tag fields even for type parameters that
     * don't use Miniboxed representation because we may forward form
     * specialized overloads that receive Miniboxed arguments.
     */
    val typeTagMap: List[(Symbol, Symbol)] =
      (for (tparam <- clazz.typeParams if spec(tparam) == Miniboxed) yield {
        val sym = sClass.newValue(typeTagName(tparam), sClass.pos, SYNTHETIC | PARAMACCESSOR | PrivateLocal)
        sym setInfo ByteClass.tpe
        sym setFlag MINIBOXED

        sClassDecls enter sym
        (pmap(tparam), sym)
      })
    // Record the new mapping for type tags to the fields carrying them
    globalTypeTags(sClass) = typeTagMap.toMap

    // adding the type tags as constructor arguments
    // TODO: add a new constructor which calls the previous one with 0,0,0
    // TODO: remove the `evidence`: Manifest[T] field(s).
    for (ctor <- clazz.info.members.filter(sym => sym.owner == clazz && sym.isConstructor)) {
      //log(clazz + " constructor " + ctor.defString)
      val newCtor = ctor.cloneSymbol(sClass)
      newCtor setFlag MINIBOXED
      newCtor modifyInfo { info =>
        val info0 = info.asSeenFrom(sClass.tpe, ctor.owner)
        val info1 = info0.substThis(clazz, sClass) // Is this still necessary?
        val (info2, mboxedArgs) = miniboxSubst(ifaceEnv, implEnv, info1)
        val tagParams = typeTagMap map (_._2.cloneSymbol(ctor, SYNTHETIC))
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
      (for (m <- clazz.info.members if m.owner == clazz && !m.isConstructor) yield {
        val newMbr = m.cloneSymbol(sClass)
        // for fields, we mangle names:
//        if (m.isTerm && !m.isMethod && !m.isImplicit) {
//          newMbr.name = specializedName(m.name, sParamValues)
//        }
        if (m.isMethod) {
          if (base(m) == m)
            base(newMbr) = newMbr
          else
            base(newMbr) = m // TODO: This is not right...
        }
        (m, newMbr)
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
    for ((m, newMbr) <- newMembers) {
      if (m.isConstructor) { // constructor
        memberSpecializationInfo(newMbr) = SpecializedImplementationOf(m)
      } else if (m.isTerm && !m.isMethod) { // fields
        if (!m.isImplicit && !isTypeTagField(m)) {
          memberSpecializationInfo(newMbr) = SpecializedImplementationOf(m)
        }
      } else {
        if (m.isDeferred)
          memberSpecializationInfo(newMbr) = Interface()

        //log(newMbr + " from " + m + " with " + spec)
        /* Check whether the method is the one that will carry the
         * implementation. If yes, find the original method from the original
         * class from which to copy the implementation. If no, find the method
         * that will have an implementation and forward to it.
         */
        if (overloads(m)(spec) == m) {
          if (m.hasAccessorFlag) {
            memberSpecializationInfo(newMbr) = memberSpecializationInfo.get(m) match {
              case Some(ForwardTo(_, original, _, _)) =>
                FieldAccessor(newMembers(original.accessed))
              case _ =>
                global.error("Unaccounted case: " + memberSpecializationInfo.get(m)); ???
            }
          } else {
            memberSpecializationInfo.get(m) match {
              case Some(ForwardTo(_, original, _, _)) =>
                memberSpecializationInfo(newMbr) = SpecializedImplementationOf(original)
              case Some(x) =>
                global.error("Unaccounted case: " + x)
              case None =>
                memberSpecializationInfo(newMbr) = SpecializedImplementationOf(m)
            }
          }
        } else {
          val target = newMembers(overloads(m)(spec))
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
