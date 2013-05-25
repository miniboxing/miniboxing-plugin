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
      log("Specializing " + sym + "...")
      tpe match {
        case PolyType(tArgs, ClassInfoType(parents, decls, typeSym)) =>
          val clazz = sym
          // Create a list of partial specializations
          val envs = specializations(tpe.typeParams)

          // For each method in the original class, add its specialized overloads
          widenClass(clazz, envs)

          log("-------------- ORIGINAL CLASS ----------------")
          log(clazz.defString)
          for (decl <- clazz.info.decls.toList.sortBy(_.nameString))
            log(f"  ${decl.defString}%80s    => ${memberSpecializationInfo.get(decl)}")

          // Create the actual specialized classes
          val classes = envs map (specializeClass(clazz, _))

          classes foreach { cls =>
            log("------------ SPEC CLASS ------------")
            log(cls.defString);
            for (decl <- cls.info.decls.toList.sortBy(_.nameString))
              log(f"  ${decl.defString}%80s    => ${memberSpecializationInfo.get(decl)}")
          }

          // Now we remove the fields from the class and leave the getters and setters as abstract
          removeClassFields(clazz)

          log("-------------- INTERFACE MEMBERS ----------------")
          for (decl <- clazz.info.decls.toList.sortBy(_.nameString))
            log(f"  ${decl.defString}%80s    => ${memberSpecializationInfo.get(decl)}")

          log("-------------- TEMPLATE MEMBERS ----------------")
          for (decl <- templateMembers.toList.sortBy(_.nameString))
            log(f"  ${decl.defString}%80s    => ${memberSpecializationInfo.get(decl)}")

          sys.exit(0);

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
    // we only specialize the members that are defined in the current class
    val members = clazz.info.members.filter(_.owner == clazz).toList

    val methods = members.filter(s => s.isMethod && !(s.isConstructor || s.isGetter || s.isSetter))
    val ctors = members.filter(_.isConstructor)
    val getters = members.filter(_.isGetter)
    val setters = members.filter(_.isSetter)
    val fields = members.filter(m => m.isTerm && !m.isMethod)

    /*
     * Add type tag fields for each parameter. Will be copied in specialized
     * subclasses.
     *
     * NOTE: We need type tag fields even for type parameters that
     * don't use Miniboxed representation because we may forward form
     * specialized overloads that receive Miniboxed arguments.
     */
    val typeTagMap: Map[Symbol, Symbol] =
      (for (tparam <- clazz.typeParams) yield {
        val sym = clazz.newValue(typeTagName(tparam), clazz.pos, SYNTHETIC | PARAMACCESSOR | PrivateLocal)
        sym setInfo ByteClass.tpe
        sym setFlag MINIBOXED

        clazz.info.decls enter sym
        (tparam, sym)
      }).toMap
    typeTags(clazz) = typeTagMap

    // adding the type tags as constructor arguments
    // TODO: add a new constructor which calls the previous one with 0,0,0
    for (ctor <- ctors) {
      ctor modifyInfo { info =>
        // TODO: Treat curried constructors
        val tagParams = typeTagMap.values map (_.cloneSymbol(ctor, SYNTHETIC))
        MethodType(info.params ++ tagParams, clazz.tpe)
      }
    }

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
          newMbr modifyInfo (info =>
            subst(env, info.asSeenFrom(newMbr.owner.thisType, newMbr.owner)) match {
              case MethodType(params, result) =>
                val tagParams = typeTagMap.values map (_.cloneSymbol(newMbr, SYNTHETIC))
                MethodType(info.params ++ tagParams, result)
              case nmt: NullaryMethodType =>
                nmt
            })

          clazz.info.resultType.decls enter (newMbr)
        }

        overloadsOfMember(spec) = newMbr
        overloads(newMbr) = overloadsOfMember
      }

      for (spec <- specs; newMbr <- overloadsOfMember get spec) {
        memberSpecializationInfo(newMbr) = genForwardingInfo(typeTagMap, newMbr, member)
      }
    }
  }

  /*
   * Substitute the type parameters with their value as given by the 'env'
   * in the type 'tpe'. Also replace use only the specialized interface
   * in signatures.
   */
  private def subst(env: TypeEnv, tpe: Type): Type = {
    val (keys, values) = env.toList.unzip
    val substMap = new SubstTypeMap(keys, values) {
      override def mapOver(tp: Type): Type = tp match {
        // TODO: Shallow type replacements.
        //        This is probably redundant at this point -- we don't need to redirect the main class to the interface
        //        case TypeRef(pre, sym, args) if (isSpecializableClass(sym)) =>
        //          val iface = baseClass(sym)
        //          TypeRef(pre, iface, mapOverArgs(args, iface.typeParams))
        //        case TypeRef(pre, sym, args) if (sym == ArrayClass) =>
        //          AnyClass.tpe // arrays are 'erased' to Any
        case _ =>
          super.mapOver(tp)
      }
    }

    substMap(tpe)
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
    clazz.setFlag(ABSTRACT)
    clazz.setFlag(TRAIT)
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

    /*
     * `pmap` is a map from the parameters of the original class to those
     * of the current specialized version.
     */
    val pmap = ParamMap(clazz.typeParams, sClass)

    /*
     * When copying information from the original class, we need to change
     * types. For the fields, we need to convert them to the new
     * representation (implEnv). For the methods, since we already have
     * one overload for each representation, we only need to change the
     * type parameter symbols to the fresh ones (ifaceEnv).
     */
    val implEnv: TypeEnv = spec map {
      case (p, Boxed)     => (p, pmap(p).tpe)
      case (p, Miniboxed) => (p, LongClass.tpe)
    }
    val ifaceEnv: TypeEnv = pmap mapValues (_.tpe)

    /*
     * Insert the newly created symbol in our various maps that are used by
     * the tree transformer.
     */
    specializedClasses(clazz) ::= sClass
    typeEnv(sClass) = implEnv
    partialSpec(sClass) = spec

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
        t => (subst(ifaceEnv, t))
      }

      // parameters which are not fixed
      val newTParams: List[Symbol] = pmap.values.toList
      GenPolyType(newTParams, ClassInfoType(sParents, sClassDecls, sClass))
    }
    sClass setInfo specializedInfoType

    // TODO: remove the `evidence`: Manifest[T] field.

    // Copy the members of the original class to the specialized class.
    val newMembers: Map[Symbol, Symbol] =
      (for (m <- clazz.info.members if m.owner == clazz) yield {
        val newMbr = m.cloneSymbol(sClass)
        // for fields, we mangle names:
        if (m.isTerm && !m.isMethod)
          newMbr.name = specializedName(m.name, sParamValues)
        (m, newMbr)
      }).toMap

    // Record the new mapping for type tags to the fields carrying them
    val typeTagMap = typeTags(clazz) map { case (p, tag) => (pmap(p), newMembers(tag)) }
    typeTags(sClass) = typeTags(clazz) mapValues newMembers

    // Replace the info in the copied members to reflect their new class
    for ((m, newMbr) <- newMembers) {
      newMbr setFlag MINIBOXED
      newMbr modifyInfo { info =>
        val info1 = info.substThis(clazz, sClass)
        if (m.isConstructor) { // constructor - add type tags as parameters
          MethodType(subst(implEnv, info1).params, newMbr.owner.tpe)
        } else if (m.isTerm && !m.isMethod) {
          if (m.isImplicit) info1 else {
            subst(implEnv, info1)
          }
        } else {
          subst(ifaceEnv, info1)
        }
      }
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
              case Some(ForwardTo(original, _, _)) =>
                FieldAccessor(newMembers(original.accessed))
              case _ =>
                global.error("Unaccounted case: " + memberSpecializationInfo.get(m)); ???
            }
          } else {
            memberSpecializationInfo.get(m) match {
              case Some(ForwardTo(original, _, _)) =>
                memberSpecializationInfo(newMbr) = SpecializedImplementationOf(original)
              case Some(x) =>
                global.error("Unaccounted case: " + x)
              case None =>
                memberSpecializationInfo(newMbr) = SpecializedImplementationOf(m)
            }
          }
        } else {
          val target = newMembers(overloads(m)(spec))
          memberSpecializationInfo(newMbr) = genForwardingInfo(typeTagMap, newMbr, target)
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
  private def genForwardingInfo(tags: Map[Symbol, Symbol], wrapper: Symbol, target: Symbol): ForwardTo = {
    def genCastInfo(srcType: Type, tgtType: Type): CastInfo = {
      val srcTypeSymbol: Symbol = srcType.typeSymbol
      val tgtTypeSymbol: Symbol = tgtType.typeSymbol

      if (srcTypeSymbol == LongClass && tgtTypeSymbol != LongClass) {
        CastMiniboxToBox(tags(tgtTypeSymbol))
      } else if (srcTypeSymbol != LongClass && tgtTypeSymbol == LongClass) {
        CastBoxToMinibox
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
        log(wrapper + ": " + wrapper.tpe)
        log(target + ": " + target.tpe)
        log(srcTypeSymbol)
        log(tgtTypeSymbol)
        log("A cast which is neither boxing, nor unboxing when handling `ForwardTo`.")
        log(srcTypeSymbol + " --> " + tgtTypeSymbol)
        ???
      }

    }

    val paramCasts = (wrapper.tpe.paramTypes zip target.tpe.paramTypes) map {
      case (wtp, ttp) =>
        genCastInfo(wtp, ttp)
    }
    val retCast = genCastInfo(target.tpe.resultType, wrapper.tpe.resultType)

    ForwardTo(target, retCast, paramCasts)
  }
}
