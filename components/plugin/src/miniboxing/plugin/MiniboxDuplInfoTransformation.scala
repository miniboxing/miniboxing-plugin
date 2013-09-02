package miniboxing.plugin

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.InfoTransform
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.tools.nsc.typechecker.Analyzer
import scala.collection.immutable.ListMap

trait MiniboxDuplInfoTransformation extends InfoTransform {
  self: MiniboxDuplComponent =>

  import global._
  import Flags._
  import definitions._

  // shamelessly stolen from specialization
  private def unspecializableClass(tp: Type) = (
       definitions.isRepeatedParamType(tp)  // ???
    || tp.typeSymbol.isJavaDefined
    || tp.typeSymbol.isPackageClass
  )

  /** Type transformation. It is applied to all symbols, compiled or loaded.
   *  If it is a 'no-specialization' run, it is applied only to loaded symbols. */
  override def transformInfo(sym: Symbol, tpe: Type): Type = {
    try {
      tpe.resultType match {
        case cinfo @ ClassInfoType(parents, decls, origin) if !unspecializableClass(tpe) =>
          val tparams  = tpe.typeParams
          if (tparams.isEmpty)
            afterMinibox(parents map (_.typeSymbol.info))
          specialize(origin, cinfo, typeEnv.getOrElse(origin, EmptyTypeEnv))
        case _ =>
          tpe
      }
    } catch {
      case t: Throwable =>
        t.printStackTrace(System.err)
        throw t
    }
  }

  def separateTypeTagArgsInTree(args: List[Tree]): (List[Tree], List[Tree]) = args match {
    case ttarg :: rest if ttarg.symbol.name.toString.endsWith("_TypeTag") =>
      val (ttargs, args) = separateTypeTagArgsInTree(rest)
      (ttarg :: ttargs, args)
    case _ => (Nil, args)
  }

  def separateTypeTagArgsInType(tpe: Type, tpArgs: List[Type], preserveTags: Boolean = true, preserveParams: Boolean = false): (List[Symbol], List[Symbol]) = tpe match {
    case MethodType(args, _) => separateTypeTagArgsInArgs(args)
    case PolyType(formals, ret) =>
      val preserveRes = separateTypeTagArgsInType(ret, Nil)
      val modifyRes = separateTypeTagArgsInType(ret.instantiateTypeParams(formals, tpArgs), Nil)
      (if (preserveTags) preserveRes._1 else modifyRes._2, if (preserveParams) preserveRes._2 else modifyRes._2)
    case _ => (Nil, Nil)
  }

  def separateTypeTagArgsInArgs(args: List[Symbol]): (List[Symbol], List[Symbol]) = args match {
    case ttarg :: rest if ttarg.name.toString.endsWith("_TypeTag") =>
      val (ttargs, args) = separateTypeTagArgsInArgs(rest)
      (ttarg :: ttargs, args)
    case _ => (Nil, args)
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
            inheritedDeferredTypeTags(current) ++= primaryDeferredTypeTags(sym1).mapValues(s => localTParamMap.getOrElse(s, s)) ++
                                                   inheritedDeferredTypeTags(sym1).mapValues(s => localTParamMap.getOrElse(s, s))
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

  class SubstSkolemsTypeMap(from: List[Symbol], to: List[Type]) extends SubstTypeMap(from, to) {
    protected override def matches(sym1: Symbol, sym2: Symbol) =
      if (sym2.isTypeSkolem) sym2.deSkolemize eq sym1
      else sym1 eq sym2
  }

  case class MiniboxSubst(env: TypeEnv) extends TypeMap {
    val (keys, shallowTypes) = env.toList.unzip
    val deepTypes = shallowTypes.map(_.filterAnnotations(_.tpe != StorageClass.tpe))
    val deepSubst = new SubstSkolemsTypeMap(keys, deepTypes)
    val shallowSubst = new SubstSkolemsTypeMap(keys, shallowTypes) {
      override def mapOver(tp: Type) = {
        val res = tp match {
          case TypeRef(pre, sym, args) if (!keys.contains(sym)) =>
            deepSubst(tp)
          case _ =>
            super.mapOver(tp)
        }
//        println(tp + " ==> " +res)
        res
      }
    }
    def apply(tp: Type) = shallowSubst(tp)
    def mapOverDeep(tp: Type) = deepSubst(tp)
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
      if (mbr.isMethod) mbr.setFlag(DEFERRED)
      if ((mbr.isTerm && !mbr.isMethod) || (mbr.isConstructor))
        decls unlink mbr
    }
    // Remove the tailcall notation from members
    decls.foreach(_.removeAnnotation(TailrecClass))
    // This needs to be delayed until trees have been duplicated, else
    // instantiation will fail, since C becomes an abstract class
    if (clazz.hasFlag(TRAIT))
      originalTraitFlag += clazz
    clazz.setFlag(TRAIT | ABSTRACT)
    decls
  }


  // TODO: Implement outerEnv -- see
  def specialize(origin: Symbol, originTpe: ClassInfoType, outerEnv: TypeEnv): Type = {

    def specialize1(pspec: PartialSpec, decls: Scope): Symbol = {

      val specParamValues = typeParamValues(origin, pspec)
      val baseName = if (flag_loader_friendly) newTermName(origin.name.toString + "_class") else origin.name
      val specName = specializedName(baseName, specParamValues).toTypeName
      val bytecodeClass = origin.owner.info.decl(specName)
      bytecodeClass.info // TODO: we have 5054 here, but even this doesn't work
      val spec = origin.owner.newClass(specName, origin.pos, origin.flags)

      spec.sourceFile = origin.sourceFile
      currentRun.symSource(spec) = origin.sourceFile
      baseClass(spec) = origin

      val pmap = ParamMap(origin.typeParams, spec)
      typeParamMap(spec) = pmap.map(_.swap).toMap

      val env: TypeEnv = pspec.map {
        case (p, Boxed)     => (p, pmap(p).tpeHK)
        case (p, Miniboxed) => (p, storageType(pmap(p)))
      }

      // Insert the newly created symbol in our various maps that are used by
      // the tree transformer.
      specializedClasses(origin) += pspec -> spec
      typeEnv(spec) = env
      partialSpec(spec) = pspec

      // declarations inside the specialized class - to be filled in later
      val specScope = newScope
      inheritedDeferredTypeTags(spec) = HashMap()
      primaryDeferredTypeTags(spec) = HashMap()

      // create the type of the new class
      val localPspec: PartialSpec = pspec.map({ case (t, sp) => (pmap(t), sp)}) // Tsp -> Boxed/Miniboxed
      val specializeParents = specializeParentsTypeMapForSpec(spec, origin, localPspec)
      val specializedTypeMap = MiniboxSubst(env)
      val specializedInfoType: Type = {
        val sParents = (origin.info.parents ::: List(origin.tpe)) map {
          t => specializedTypeMap(t)
        } map specializeParents

        val newTParams: List[Symbol] = origin.typeParams.map(pmap)
        GenPolyType(newTParams, ClassInfoType(sParents, specScope, spec))
      }
      afterMinibox(spec setInfo specializedInfoType)

      // Add type tag fields for each parameter. Will be copied in specialized subclasses.
      val typeTagMap: List[(Symbol, Symbol)] =
        (for (tparam <- origin.typeParams if tparam.hasFlag(MINIBOXED) && pspec(tparam) == Miniboxed) yield {
          val sym =
            if (origin.isTrait)
              spec.newMethodSymbol(typeTagName(spec, tparam), spec.pos, DEFERRED).setInfo(MethodType(List(), ByteTpe))
            else
              spec.newValue(typeTagName(spec, tparam), spec.pos, PARAMACCESSOR | PrivateLocal).setInfo(ByteTpe)

          sym setFlag MINIBOXED
          if (origin.isTrait) {
            primaryDeferredTypeTags(spec) += sym -> pmap(tparam)
            memberSpecializationInfo(sym) = DeferredTypeTag(tparam)
          }

          specScope enter sym
          (pmap(tparam), sym)
        })

      // Record the new mapping for type tags to the fields carrying them
      globalTypeTags(spec) = typeTagMap.toMap

      // Copy the members of the original class to the specialized class.
      val newMembers: Map[Symbol, Symbol] =
        // we only duplicate methods and fields
        (for (mbr <- decls.toList if (!notSpecializable(origin, mbr) && !(mbr.isModule || mbr.isType || mbr.isConstructor))) yield {
          val newMbr = mbr.cloneSymbol(spec)
          if (mbr.isMethod) {
            if (base(mbr) == mbr)
              base(newMbr) = newMbr
            else
              base(newMbr) = mbr
          }
          val update = (mbr.info.params zip newMbr.info.params).toMap
          localTypeTags(newMbr) = localTypeTags.getOrElse(mbr, Map.empty).map({ case (tpe, tag) => (tpe, update(tag))})
          globalTypeTags(newMbr) = globalTypeTags(spec)
          (mbr, newMbr)
        }).toMap

      // Replace the info in the copied members to reflect their new class
      for ((m, newMbr) <- newMembers if !m.isConstructor) {

        newMbr setFlag MINIBOXED
        newMbr modifyInfo { info =>

          val info0 = info.asSeenFrom(spec.tpe, m.owner)
          val info1 = info0.substThis(origin, spec)
          val info2 =
            if (m.isTerm && !m.isMethod) {
              // this is where we specialize fields:
              specializedTypeMap(info1)
            } else
              info1

          info2
        }

        // TODO: Is this okay?
        typeEnv(newMbr) = typeEnv(spec)
        localTypeTags(newMbr) = localTypeTags.getOrElse(m, Map.empty).map(p => pmap(p._1)).zip(newMbr.info.params).toMap
        debug(spec + " entering: " + newMbr)
        specScope enter newMbr
      }

      // adding the type tags as constructor arguments
      for (ctor <- decls.filter(sym => sym.name == nme.CONSTRUCTOR)) {
        val newCtor = ctor.cloneSymbol(spec)
        newCtor setFlag MINIBOXED
        newCtor modifyInfo { info =>
          val info0 = info.asSeenFrom(spec.tpe, ctor.owner)
          val info1 = info0.substThis(origin, spec) // Is this still necessary?
          val info2 = specializedTypeMap(info1)
          val tagParams = typeTagMap map (_._2.cloneSymbol(newCtor, 0))
          localTypeTags(newCtor) = typeTagMap.map(_._1).zip(tagParams).toMap
          def transformArgs(tpe: Type): Type = tpe match {
            case MethodType(params, ret) =>
              MethodType(tpe.params, transformArgs(ret))
            case TypeRef(_, _, _) =>
              spec.tpe
            case _ =>
              tpe
          }
          overloads.get(ctor) match {
            case Some(map) => map += pspec -> newCtor
            case None => overloads(ctor) = HashMap(pspec -> newCtor)
          }
          val info3 = transformArgs(info2)
          MethodType(tagParams.toList ::: info3.params, info3.resultType)
        }
        memberSpecializationInfo(newCtor) = SpecializedImplementationOf(ctor)
        specScope enter newCtor
      }

      // Record how the body of these members should be generated
      for ((mbr, newMbr) <- newMembers) {
        if (mbr.isConstructor || (mbr.isTerm && !mbr.isMethod)) {
          memberSpecializationInfo(newMbr) = SpecializedImplementationOf(mbr)
        } else {
          if (mbr.isDeferred)
            memberSpecializationInfo(newMbr) = Interface()
          else {
            // Check whether the method is the one that will carry the
            // implementation. If yes, find the original method from the original
            // class from which to copy the implementation. If no, find the method
            // that will have an implementation and forward to it.
            if (overloads(mbr).isDefinedAt(pspec)) {
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
                newMbr.removeAnnotation(TailrecClass) // can't be a tailcall if you're fwding
                memberSpecializationInfo(newMbr) = genForwardingInfo(newMbr, wrapTagMap, target, targTagMap)
              }
            } else {
              memberSpecializationInfo(newMbr) = SpecializedImplementationOf(mbr)
            }
          }
        }
      }

      // populate the overloads data structure for the new members also
      for ((m, newMbr) <- newMembers if (m.isMethod && !m.isConstructor)) {
        if (overloads(m).isDefinedAt(pspec)) {
          val newMbrMeantForSpec = newMembers(overloads(m)(pspec))
          if (!(overloads isDefinedAt newMbrMeantForSpec)) {
            overloads(newMbrMeantForSpec) = new HashMap[PartialSpec, Symbol]
            for ((s, m) <- overloads(m)) {
              overloads(newMbrMeantForSpec)(s) = newMembers(m)
            }
          }
          overloads(newMbr) = overloads(newMbrMeantForSpec)
        } else
          // member not specialized:
          overloads(newMbr) = HashMap.empty
      }

      // specialized overloads
      addSpecialOverrides(pspec, localPspec, spec, specScope, inPlace = true)

      // deferred type tags:
      addDeferredTypeTagImpls(spec, specScope, inPlace = true)

      // normalized members:
      normalizeMembers(spec, specScope, inPlace = true)

      spec
    }

    def widen(specs: List[PartialSpec]): List[Symbol] = {

      baseClass(origin) = origin
      typeParamMap(origin) = origin.info.typeParams.map((p: Symbol) => (p, p)).toMap
      inheritedDeferredTypeTags(origin) = HashMap()
      primaryDeferredTypeTags(origin) = HashMap()
      specializedClasses(origin) = HashMap()

      // we only specialize the members that are defined in the current class
      val members = origin.info.members.filter(_.owner == origin).toList
//      members foreach (_ info)

      val methods = members.filter(s => s.isMethod && !(s.isConstructor || s.isGetter || s.isSetter))
      val getters = members.filter(_.isGetter)
      val setters = members.filter(_.isSetter)
      val fields = members.filter(m => m.isTerm && !m.isMethod)

      var newMembers = List[Symbol]()

      // we make specialized overloads for every member of the original class
      for (member <- methods ::: getters ::: setters if !notSpecializable(origin, member)) {
        val overloadsOfMember = new HashMap[PartialSpec, Symbol]
        val specs_filtered =  if (needsSpecialization(origin, member)) specs else specs.filter(PartialSpec.isAllAnyRef(_))

        for (spec <- specs_filtered) {
          var newMbr = member
          if (!PartialSpec.isAllAnyRef(spec)) {
            val env: TypeEnv = spec map {
              case (p, v) => (p, if (v == Boxed) p.tpe else storageType(p))
            }
            val specializedTypeMap = MiniboxSubst(env)

            newMbr = member.cloneSymbol(origin)

            newMbr setFlag MINIBOXED
            newMbr setName (specializedName(member.name, typeParamValues(origin, spec)))
            newMbr modifyInfo (info => {
              val info0 = specializedTypeMap(info.asSeenFrom(newMbr.owner.thisType, newMbr.owner))
              val localTags =
                for (tparam <- origin.typeParams if tparam.hasFlag(MINIBOXED) && spec(tparam) == Miniboxed)
                  yield (tparam, newMbr.newValue(shortTypeTagName(tparam), newMbr.pos).setInfo(ByteClass.tpe))
              localTypeTags(newMbr) = localTags.toMap
              val tagParams = localTags.map(_._2)
              val info1 =
                info0 match {
                  case MethodType(args, ret) =>
                    MethodType(tagParams ::: args, ret)
                  case PolyType(targs, MethodType(args, ret)) =>
                    val ntargs = targs.map(_.cloneSymbol(newMbr))
                    val tpe = MethodType(tagParams ::: args, ret).substSym(targs, ntargs)
                    assert((tagParams ::: args).length == tpe.params.length, tagParams + ", " + args + ", " + tpe.params)
                    val update = ((tagParams ::: args) zip tpe.params).toMap
                    localTypeTags(newMbr) = localTypeTags(newMbr).map({ case (t, tag) => (t, update(tag)) })
                    PolyType(ntargs, tpe)
                  case _ => ???
                }

              // TODO: We need to clear some override flags, but that's not critical for the classes to work (#41)

              info1
            })

            // rewire to the correct referenced symbol, else mixin crashes
            val alias = newMbr.alias
            if (alias != NoSymbol) {
              // Rewire alias:
              val baseTpe = origin.info.baseType(alias.owner)
              val pspec2 = ((baseTpe.typeSymbol.typeParams zip baseTpe.typeArgs) flatMap {
                case (param, tpe) if param.hasFlag(MINIBOXED) =>
                  if (ScalaValueClasses.contains(tpe.typeSymbol))
                    Some((param, Miniboxed))
                  else if (spec.get(tpe.typeSymbol) == Some(Miniboxed))
                    Some((param, Miniboxed))
                  else
                    Some((param, Boxed))
                case _ =>
                  None
              }).toMap
              if (overloads.isDefinedAt(alias) && overloads(alias).isDefinedAt(pspec2)) {
                newMbr.asInstanceOf[TermSymbol].referenced = overloads(alias)(pspec2)
              } else {
                log("Could not rewire!")
                log("base: " + newMbr.defString)
                log("from: " + alias.defString)
                log(baseTpe)
                log(baseTpe.typeSymbol.typeParams)
                log(baseTpe.typeArgs)
                log(pspec2)
                log(overloads.get(alias))
                log()
              }
            }

            newMembers ::= newMbr
          }

          overloadsOfMember(spec) = newMbr
          overloads(newMbr) = overloadsOfMember
          base(newMbr) = member
        }

        for (spec <- specs; newMbr <- overloadsOfMember get spec)
          memberSpecializationInfo(newMbr) = genForwardingInfo(newMbr, localTypeTags.getOrElse(newMbr, Map.empty), member, Map.empty)
      }

      // TODO: Do we want to keep overrides?
      origin.info.decls.toList ++ newMembers
    }

    // create overrides for specialized variants of the method
    def addSpecialOverrides(globalPSpec: PartialSpec, pspec: PartialSpec, clazz: Symbol, scope: Scope, inPlace: Boolean = false): Scope = {

      val scope1 = if (inPlace) scope else scope.cloneScope
      val base = baseClass.getOrElse(clazz, NoSymbol)

      def specializedOverriddenMembers(sym: Symbol): Symbol = {
        for (baseOSym <- sym.allOverriddenSymbols) {
          if (sym.typeParams.nonEmpty) {
            val tparamMap = (baseOSym.typeParams zip sym.typeParams).toMap
            val tparamMiss = baseOSym.typeParams.filter(tparam =>
              isSpecialized(baseOSym.owner, tparam) && !isSpecialized(clazz, tparamMap(tparam))).map(tparamMap)
            if (tparamMiss.nonEmpty)
              currentUnit.error(sym.pos, "The " + sym + " in " + clazz + " overrides " + baseOSym + " in " + baseOSym.owner + " therefore needs to have the follwing type parameters marked with @miniboxed: " + tparamMiss.mkString(",") + ".")
          }

          if (isSpecializableClass(baseOSym.owner) && base != baseOSym.owner) {
            // here we get the base class, not the specialized class
            // therefore, the 1st step is to identify the specialized class
            val baseParent = baseOSym.owner
            val baseParentTpe = clazz.info.baseType(baseParent)
            val spec = PartialSpec.fromTypeInContext(baseParentTpe.asInstanceOf[TypeRef], pspec)
            if (!PartialSpec.isAllAnyRef(spec) && overloads.isDefinedAt(baseOSym)) {
              overloads(baseOSym).get(spec) match {
                case Some(mainSym) =>
                  return mainSym
                case _ =>
              }
            }
          }
        }
        NoSymbol
      }

    if (clazz.isClass)
      for (sym <- scope1 if sym.isMethod && !sym.isConstructor) {
        specializedOverriddenMembers(sym).toOption.foreach(oSym => {
          val localOverload = overloads.get(sym).flatMap(_.get(globalPSpec)).getOrElse(NoSymbol)
          // only generate the override if we don't have an overload which matches the current symbol:
          // matching the symbol is a pain in the arse, since oSym points to the interface while localOverload
          // points to the current clazz -- TODO: we could carry newMembers and get the correspondence
          if (localOverload.name != oSym.name) {
            val overrider = oSym.cloneSymbol(clazz)
            overrider.setInfo(oSym.info.cloneInfo(overrider).asSeenFrom(clazz.info, oSym.owner))
            overrider.resetFlag(DEFERRED).setFlag(OVERRIDE)

            val paramUpdate = (oSym.info.params zip overrider.info.params).toMap
            val baseClazz = oSym.owner
            val baseType = clazz.info.baseType(baseClazz)
            val tparamUpdate = (baseClazz.typeParams zip baseType.typeArgs.map(_.typeSymbol)).toMap
            val typeTags = globalTypeTags.getOrElse(clazz, Map.empty) ++
                           localTypeTags.getOrElse(oSym, Map.empty).map({ case (tpe, tag) => (tparamUpdate(tpe), paramUpdate(tag))})

            // direct to the most specific override instead of the generic member:
            localTypeTags(overrider) = typeTags
            memberSpecializationInfo(overrider) =
              if (localOverload == NoSymbol)
                genForwardingInfo(overrider, typeTags, sym, Map.empty, overrider = true)
              else {
                val targTagMap = localTypeTags.getOrElse(localOverload, Map.empty)
                genForwardingInfo(overrider, typeTags, localOverload, targTagMap, overrider = true)
              }

            scope1 enter overrider
          }
        })
      }
    scope1
  }

    // expand methods with specialized members
    def normalizeMembers(clazz: Symbol, scope: Scope, inPlace: Boolean = false): Scope = {
      val scope1 = if (inPlace) scope else scope.cloneScope

      val newMemberSets =
        for (member <- scope if !notSpecializable(clazz, member) && member.isMethod && member.info.typeParams.exists(isSpecialized(clazz, _))) yield {
          val tparams = member.typeParams.filter(isSpecialized(clazz, _))
          tparams foreach (_ setFlag MINIBOXED)
          val pspecs = specializations(tparams)
          val normalizationsOfMember = new HashMap[PartialSpec, Symbol]

          val env = typeEnv.getOrElse(member, EmptyTypeEnv)
          val specializedTypeMap = MiniboxSubst(env)
          val newMembers = (for (pspec <- pspecs) yield {
            var newMbr = member
            if (!PartialSpec.isAllAnyRef(pspec)) {

              newMbr = member.cloneSymbol(clazz)
              newMbr setFlag MINIBOXED
              newMbr setName (specializedName(newTermName(member.name.toString + "_n"), typeParamValues(member, pspec)))
              newMbr modifyInfo (info0 => {
                info0.typeParams.foreach(_.removeAnnotation(MinispecClass))
                val deepEnv: Map[Symbol, Symbol] = member.typeParams.zip(info0.typeParams).toMap
                val normalizedEnv =
                  pspec flatMap {
                    case (p, Boxed)     => None // stays the same
                    case (p, Miniboxed) => Some((deepEnv(p), storageType(deepEnv(p))))
                  }
                val info1 = specializedTypeMap(info0.resultType)
                typeParamMap(newMbr) = deepEnv.map(_.swap).toMap
                typeEnv(newMbr) = env ++ normalizedEnv
                val localTags =
                  for (tparam <- member.typeParams if tparam.hasFlag(MINIBOXED) && pspec(tparam) == Miniboxed)
                    yield (deepEnv(tparam), newMbr.newValue(shortTypeTagName(tparam), newMbr.pos).setInfo(ByteClass.tpe))
                val updateParams = (member.info.params zip info1.params).toMap
                val oldLocalTypeTags = localTypeTags.getOrElse(member, Map())
                val updLocalTypeTags = oldLocalTypeTags.map({ case (tpe, tag) => (tpe, updateParams(tag))})
                localTypeTags(newMbr) = updLocalTypeTags ++ localTags
                normalSpec(newMbr) = pspec.map({ case (tp, state) => (deepEnv(tp), state)})
                val tagParams = localTags.map(_._2)
                GenPolyType(info0.typeParams, MethodType(tagParams ::: info1.params, info1.resultType))
              })

              scope1 enter newMbr
            }

            normalizationsOfMember(pspec) = newMbr
            normalizations(newMbr) = normalizationsOfMember
            normbase(newMbr) = member

            (pspec, newMbr)
          })

          (member, newMembers)
        }

      for ((member, newMembers) <- newMemberSets)
        for ((pspec, newMbr) <- newMembers if newMbr != member)
          memberSpecializationInfo(newMbr) =
            memberSpecializationInfo.get(member) match {
              case Some(Interface()) =>
                Interface()

              case Some(SpecializedImplementationOf(baseMbr)) =>
                val env = typeEnv.getOrElse(newMbr, EmptyTypeEnv)
                val update = member.typeParams.zip(baseMbr.typeParams).toMap
                // TODO: This probably needs a smarter update
                val newDeepEnv = env.map({case (tparam, tpe) => (update.getOrElse(tparam, tparam), tpe)})
                typeEnv(newMbr) = newDeepEnv
                SpecializedImplementationOf(baseMbr)

              case Some(ForwardTo(_, target0, _, _)) =>

                // take the normalized version of the target
                val updateTparam = (member.typeParams zip target0.typeParams).toMap
                val pspecUpd = pspec.map({ case (tpar, status) => (updateTparam(tpar), status)})
                val target = normalizations.get(target0).flatMap(_.get(pspecUpd)).getOrElse(target0)

                val globalTypeTagsMap = typeEnv.getOrElse(clazz, EmptyTypeEnv)
                val globalTypeTagsUpd = globalTypeTags.getOrElse(clazz, Map.empty).map({case (tpar, tag) => (globalTypeTagsMap.getOrElse(tpar, tpar.tpe).typeSymbol, tag)})
                val updateTparam2 = (newMbr.typeParams zip target.typeParams).toMap
                val wrapperTypeTags = globalTypeTagsUpd ++ localTypeTags(newMbr) ++
                                      localTypeTags(newMbr).map({ case (tpar, tag) => (updateTparam2.getOrElse(tpar, tpar), tag)})

                val targetTypeTags = localTypeTags.getOrElse(target, Map())
                // println("ORIGIN: " + member.defString)
                // println("FROM:   " + newMbr.defString)
                // println("TO:     " + target.defString)
                // println(wrapperTypeTags)
                // println(targetTypeTags)

                genForwardingInfo(newMbr, wrapperTypeTags, target, targetTypeTags)

              case None =>
                SpecializedImplementationOf(member)

              case _ =>
                println("Unknown info for " + member.defString)
                println("when specializing " + newMbr.defString + ":")
                println(memberSpecializationInfo.get(member))
                ???
            }

      scope1
    }

    // add members derived from deferred type tags
    def addDeferredTypeTagImpls(origin: Symbol, scope: Scope, inPlace: Boolean = false): Scope = {
      val scope1 = if (inPlace) scope else scope.cloneScope
      if (!origin.isTrait) {
        val deferredTags = primaryDeferredTypeTags(origin) ++ inheritedDeferredTypeTags(origin)
        // classes satisfy the deferred tags immediately, no need to keep them
        for ((method, tparam) <- deferredTags) {
          val impl = method.cloneSymbol(origin).setFlag(MINIBOXED)
          memberSpecializationInfo(impl) = DeferredTypeTagImplementation(tparam)
          scope1 enter impl
        }
        inheritedDeferredTypeTags(origin).clear()
      }
      scope1
    }

    // begin specialize

    // force info on parents to get all specialized metadata
    afterMinibox(originTpe.parents.map(_.typeSymbol.info))
    val specs = if (isSpecializableClass(origin)) specializations(origin.info.typeParams) else Nil
    specs.map(_.map(_._1.setFlag(MINIBOXED))) // TODO: Only needs to be done once per type parameter

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
      val scope3 = normalizeMembers(origin, scope2)

      log("  // interface:")
      log("  " + origin.defString + " {")
      for (decl <- scope3.toList.sortBy(_.defString))
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
      origin.resetFlag(CASE)

      GenPolyType(origin.info.typeParams, ClassInfoType(parents1, scope3, origin))
    } else {
      inheritedDeferredTypeTags(origin) = HashMap()
      primaryDeferredTypeTags(origin) = HashMap()
      val scope1 = originTpe.decls.cloneScope
      val specializeTypeMap = specializeParentsTypeMapForGeneric(origin)
      val parents1 = originTpe.parents map specializeTypeMap
      val scope2 = addSpecialOverrides(Map.empty, Map.empty, origin, scope1)
      val scope3 = addDeferredTypeTagImpls(origin, scope2)
      val scope4 = normalizeMembers(origin, scope3)
      GenPolyType(origin.info.typeParams, ClassInfoType(parents1, scope4, origin))
    }
  }

  /**
   * Generate the information about how arguments and return value should
   * be converted when forwarding to `target`.
   */
  private def genForwardingInfo(wrapper: Symbol, wrapperTags: Map[Symbol, Symbol], target: Symbol, targetTags: Map[Symbol, Symbol], overrider: Boolean = false): ForwardTo = {

    // TODO: only basic parameters should go here
    val (wtpe, ttpe) = (wrapper.info, target.info) match {
      case (PolyType(wtparams, wtpe), PolyType(ttparams, ttpe)) =>
        (wtpe, ttpe.instantiateTypeParams(ttparams, wtparams.map(_.tpe)))
      case (wtpe, ttpe) =>
        (wtpe, ttpe)
    }

    def genCastInfo(srcType: Type, tgtType: Type): CastInfo = {
      val srcTypeSymbol: Symbol = srcType.typeSymbol
      val tgtTypeSymbol: Symbol = tgtType.typeSymbol

      val res = if (srcTypeSymbol == LongClass && tgtTypeSymbol != LongClass)
        CastMiniboxToBox(wrapperTags(tgtTypeSymbol))
      else if (srcTypeSymbol != LongClass && tgtTypeSymbol == LongClass)
        CastBoxToMinibox(wrapperTags(srcTypeSymbol))
      else if ((srcTypeSymbol == tgtTypeSymbol) && (srcType =:= tgtType))
        NoCast
      else {
        log(wrapper + ": " + wrapper.tpe + " ==> " + wtpe)
        log(target + ": " + target.tpe + " ==> " + ttpe)
        log(srcTypeSymbol)
        log(tgtTypeSymbol)
        log("A cast which is neither boxing, nor unboxing when handling `ForwardTo`.")
        log(srcTypeSymbol)
        log(tgtTypeSymbol)
        log(srcType)
        log(tgtType)
        ???
      }
      res
    }

    def params(target: Symbol, tpe: Type, tparams: List[Symbol]): (List[Symbol], List[Symbol]) =
      if (!target.isMethod || (!target.name.toString.contains("_J") && !target.name.toString.contains("_L")))
        (Nil, tpe.params)
      else
        separateTypeTagArgsInType(tpe, tparams.map(_.tpe)) // we instantiate the type separately

    def typeTagParams = {
      val targetTParams = targetTags.map(_.swap).toMap
      val targs = params(target, target.info, wrapper.typeParams)._1.map(targetTParams)
      val tagParams = targs.map(wrapperTags)
      tagParams
    }


    val wrapParams = params(wrapper, wtpe, Nil)._2.map(_.tpe)
    val targParams = params(target, ttpe, Nil)._2.map(_.tpe)
    assert(wrapParams.length == targParams.length, (wrapParams, targParams))
    val paramCasts = (wrapParams zip targParams) map {
      case (wtp, ttp) => genCastInfo(wtp, ttp)
    }
    val retCast = genCastInfo(ttpe.finalResultType, wtpe.finalResultType)

    ForwardTo(typeTagParams, target, retCast, paramCasts)(overrider)
  }
}
