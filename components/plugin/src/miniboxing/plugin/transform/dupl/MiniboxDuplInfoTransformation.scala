package miniboxing.plugin
package transform
package dupl

import scala.tools.nsc.transform.InfoTransform
import scala.reflect.internal.Flags._
import scala.collection.mutable.HashMap

trait MiniboxDuplInfoTransformation extends InfoTransform {
  self: MiniboxDuplComponent =>

  import global._
  import definitions._

  /** Type transformation. It is applied to all symbols, compiled or loaded.
   *  If it is a 'no-specialization' run, it is applied only to loaded symbols. */
  override def transformInfo(sym: Symbol, tpe: Type): Type =
    try {
      tpe.resultType match {
        case cinfo @ ClassInfoType(parents, decls, origin) if heuristics.specializableClass(tpe) =>
          specialize(origin, cinfo)
        case _ if heuristics.normalizableMethodInMethod(sym) =>
          normalizeMembers(sym); tpe
        case _ =>
          tpe
      }
    } catch {
      case t: Throwable =>
        t.printStackTrace(System.err)
        throw t
    }

  /** Expand methods with specialized type parameters */
  def normalizeMembers(member: Symbol): List[Symbol] = {

    if (!member.isMethod || !member.hasMiniboxedTypeParameters)
      return List()

    if (!heuristics.specializableMethodInClass(member.owner, member))
      return List()

    // mark the member as the base
    metadata.setNormalStem(member, member)

    val tparams = member.getMiniboxedTypeParameters
    tparams foreach (_ setFlag MINIBOXED)
    val pspecs = variants.allSpecializations(tparams)
    val normalizedOverloads = new HashMap[PartialSpec, Symbol]

    // step1: create the normalized members
    val newMembers = (for (pspec <- pspecs) yield {
      var newMbr = member
      if (!PartialSpec.isAllAnyRef(pspec)) {

        newMbr = member.cloneSymbol(member.owner)
        newMbr setFlag MINIBOXED
        newMbr setName (specializedName(newTermName(member.name.toString + "_n"), variants.typeParamValues(member, pspec)))
        newMbr modifyInfo (info0 => {
          info0.typeParams.foreach(_.removeAnnotation(MinispecClass))

          // update the signature to include @storage
          val deepEnv: Map[Symbol, Symbol] = // <old tparam> ==> <new tparam>
            member.typeParams.zip(info0.typeParams).toMap
          val normalizedSignatureEnv =       // <new tparam> ==> @storage <new tparam>
            pspec flatMap {
              case (p, Boxed)     => None // stays the same
              case (p, Miniboxed) => Some((deepEnv(p), storageType(deepEnv(p))))
            }
          val normalizedTypeMap = typeMappers.MiniboxSubst(normalizedSignatureEnv)
          val info1 = normalizedTypeMap(info0.resultType)

          // update local type tags (inherited from class) to the new parameters
          val updateParams = (member.info.params zip info1.params).toMap
          val oldLocalTypeTags = metadata.localTypeTags.getOrElse(member, Map())
          val updLocalTypeTags = oldLocalTypeTags.map({ case (tag, tpe) => (updateParams(tag), tpe)})
          metadata.localTypeTags(newMbr) = updLocalTypeTags

          // create type tags for the method's own miniboxed parameters
          val localTags =
            for (tparam <- member.typeParams if tparam.hasFlag(MINIBOXED) && pspec(tparam) == Miniboxed)
              yield (newMbr.newValue(shortTypeTagName(tparam), newMbr.pos).setInfo(ByteClass.tpe), deepEnv(tparam))
          metadata.normalTypeTags(newMbr) = localTags.toMap

          // update signature for created type tags
          val tagParams = localTags.map(_._1)
          GenPolyType(info0.typeParams, MethodType(tagParams ::: info1.params, info1.resultType))
        })
      }

      metadata.normalSpecialization(newMbr) = pspec
      normalizedOverloads(pspec) = newMbr
      metadata.setNormalStem(newMbr, member)

      (pspec, newMbr)
    })

    metadata.normalOverloads(member) = normalizedOverloads

    // step 2: set their specialization info
    for ((pspec, newMbr) <- newMembers if newMbr != member)
      memberSpecializationInfo(newMbr) =
        memberSpecializationInfo.get(member) match {
          case Some(Interface) =>
            Interface

          case Some(SpecializedImplementationOf(stemMethodInStemClass)) =>
            SpecializedImplementationOf(stemMethodInStemClass)

          case Some(ForwardTo(target)) =>
            ForwardTo(target)(overrider = false)

          case None =>
            SpecializedImplementationOf(member)

          case specialization =>
            global.error(
                s"""|Unknown info for ${member.defString}
                    |when specializing ${newMbr.defString}:
                    |specialization""".stripMargin)
            SpecializedImplementationOf(member)
        }

    // step 3: return the new members
    newMembers.map(_._2).filterNot(_ == member)
  }


  /** Expand classes with specialized type parameters */
  def specialize(origin: Symbol, originTpe: ClassInfoType): Type = {

    def specialize1(pspec: PartialSpec, decls: Scope): Symbol = {

      val specParamValues = typeParamValues(origin, pspec)
      val baseName = if (flag_loader_friendly) newTermName(origin.name.toString + "_class") else origin.name
      val specName = specializedName(baseName, specParamValues).toTypeName
      val bytecodeClass = origin.owner.info.decl(specName)
      bytecodeClass.info // TODO: we have 5054 here, but even this doesn't work
      val spec = origin.owner.newClass(specName, origin.pos, origin.flags)

      spec.associatedFile = origin.associatedFile
      currentRun.symSource(spec) = origin.sourceFile
      specializedStemClass(spec) = origin
      spec.resetFlag(INTERFACE)

      val pmap = ParamMap(origin.typeParams, spec)
      variantParamMap(spec) = pmap.map(_.swap).toMap

      val tparamUpdate: TypeEnv = pmap.map {case (s1, s2) => (s1, s2.tpeHK)}
      val envOuter: TypeEnv = pspec.map {
        case (p, Boxed)     => (p, pmap(p).tpeHK)
        case (p, Miniboxed) => (p, storageType(pmap(p)))
      }

      val envInner: TypeEnv = pspec.flatMap {
        case (p, Miniboxed) => Some((pmap(p), storageType(pmap(p))))
        case _ => None
      }

      // Insert the newly created symbol in our various maps that are used by
      // the tree transformer.
      specializedClasses(origin) += pspec -> spec
      variantTypeEnv(spec) = envOuter
      classSpecialization(spec) = pspec

      // declarations inside the specialized class - to be filled in later
      val specScope = newScope
      inheritedDeferredTypeTags(spec) = HashMap()
      primaryDeferredTypeTags(spec) = HashMap()

      // create the type of the new class
      val localPspec: PartialSpec = pspec.map({ case (t, sp) => (pmap(t), sp)}) // Tsp -> Boxed/Miniboxed
      val specializeParents = specializeParentsTypeMapForSpec(spec, origin, localPspec)
      val specializedTypeMapOuter = MiniboxSubst(pmap.map({ case (tpSym, ntpSym) => (tpSym, ntpSym.tpeHK)}))
      val specializedTypeMapInner = MiniboxSubst(envInner)
      val specializedInfoType: Type = {
        val sParents = (origin.info.parents ::: List(origin.tpe)) map {
          t => specializedTypeMapOuter(t)
        } map specializeParents

        val newTParams: List[Symbol] = origin.typeParams.map(pmap)
        GenPolyType(newTParams, ClassInfoType(sParents, specScope, spec))
      }
      afterMiniboxDupl(spec setInfo specializedInfoType)

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
          (sym, pmap(tparam))
        })

      // Record the new mapping for type tags to the fields carrying them
      globalTypeTags(spec) = typeTagMap.toMap

      // Copy the members of the original class to the specialized class.
      val newMembers: Map[Symbol, Symbol] =
        // we only duplicate methods and fields
        (for (mbr <- decls.toList if (!notSpecializable(origin, mbr) && !(mbr.isModule || mbr.isType || mbr.isConstructor))) yield {
          val newMbr = mbr.cloneSymbol(spec)
          if (mbr.isMethod) {
            if (specializationStemMember(mbr) == mbr)
              specializationStemMember(newMbr) = newMbr
            else
              specializationStemMember(newMbr) = mbr
          }
          val update = (mbr.info.params zip newMbr.info.params).toMap
          localTypeTags(newMbr) = localTypeTags.getOrElse(mbr, Map.empty).map({ case (tag, tparam) => (update(tag), tparam)})
          globalTypeTags(newMbr) = globalTypeTags(spec)
          (mbr, newMbr)
        }).toMap

      // Replace the info in the copied members to reflect their new class
      for ((m, newMbr) <- newMembers if !m.isConstructor) {

        newMbr setFlag MINIBOXED
        newMbr modifyInfo { info =>

          val info0 = info.substThis(origin, spec)
          val info1 = info0.asSeenFrom(spec.thisType, m.owner)
          val info2 =
            if (m.isTerm && !m.isMethod) {
              // this is where we specialize fields:
              specializedTypeMapInner(info1)
            } else
              info1

          info2
        }

        // TODO: Is this okay?
        variantTypeEnv(newMbr) = variantTypeEnv(spec)
        localTypeTags(newMbr) = newMbr.info.params.zip(localTypeTags.getOrElse(m, Map.empty).map(p => pmap(p._2))).toMap
        debug(spec + " entering: " + newMbr)
        specScope enter newMbr
      }

      // adding the type tags as constructor arguments
      for (ctor <- decls.filter(sym => sym.name == nme.CONSTRUCTOR)) {
        val newCtor = ctor.cloneSymbol(spec).setPos(ctor.pos)

        newCtor setFlag MINIBOXED
        newCtor modifyInfo { info =>
          val info0 = info.asSeenFrom(spec.thisType, ctor.owner)
          val info1 = info0.substThis(origin, spec) // Is this still necessary?
          val info2 = specializedTypeMapInner(info1)
          val tagParams = typeTagMap map (_._1.cloneSymbol(newCtor, 0))
          localTypeTags(newCtor) = tagParams.zip(typeTagMap.map(_._2)).toMap
          def transformArgs(tpe: Type): Type = tpe match {
            case MethodType(params, ret) =>
              MethodType(tpe.params, transformArgs(ret))
            case TypeRef(_, _, _) =>
              spec.tpe
            case _ =>
              tpe
          }
          // add the new constructor as an overload for the original constructor
          specializedMembers.get(ctor) match {
            case Some(map) => map += pspec -> newCtor
            case None => specializedMembers(ctor) = HashMap(pspec -> newCtor)
          }
          val info3 = transformArgs(info2)

          // dummy constructor
          if (!tagParams.isEmpty) {
            val dummyCtor = ctor.cloneSymbol(spec).setPos(ctor.pos)
            dummyCtor.setInfo(info3.cloneInfo(dummyCtor))
            specializedMembers.get(dummyCtor) match {
              case Some(map) => map += pspec -> newCtor
              case None => specializedMembers(dummyCtor) = HashMap(pspec -> newCtor)
            }
            dummyConstructors += dummyCtor
            specScope enter dummyCtor
//            println("dummy constructor: " + dummyCtor.defString)
          }

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
          // Check whether the method is the one that will carry the
          // implementation. If yes, find the original method from the original
          // class from which to copy the implementation. If no, find the method
          // that will have an implementation and forward to it.
          if (specializedMembers(mbr).isDefinedAt(pspec)) {
            if (specializedMembers(mbr)(pspec) == mbr) {
              if (mbr.hasAccessorFlag) {
                memberSpecializationInfo(newMbr) = memberSpecializationInfo.get(mbr) match {
                  case Some(ForwardTo(target)) =>
                    FieldAccessor(newMembers(target.accessed))
                  case _ =>
                    global.error("Unaccounted case: " + memberSpecializationInfo.get(mbr)); ???
                }
              } else {
                memberSpecializationInfo.get(mbr) match {
                  case Some(ForwardTo(target)) =>
                    memberSpecializationInfo(newMbr) =
                      if (!mbr.isDeferred)
                        SpecializedImplementationOf(target)
                      else
                        Interface()
                  case Some(x) =>
                    global.error("Unaccounted case: " + x)
                  case None =>
                    memberSpecializationInfo(newMbr) = SpecializedImplementationOf(mbr)
                }
              }
            } else {
              // here, we're forwarding to the all-AnyRef member, knowing that the
              // redirection algorithm will direct to the appropriate member later
              val target = newMembers(specializedMembers(mbr)(pspec.isAllBoxed))
              // a forwarder will never be a tailcall itself, although it may
              // forward to a tailcall method:
              newMbr.removeAnnotation(TailrecClass)
              // since the member will be a forwarder, it can't be DEFERRED (See #85):
              newMbr.resetFlag(DEFERRED)
              memberSpecializationInfo(newMbr) = genForwardingInfo(target)
            }
          } else {
            memberSpecializationInfo(newMbr) = SpecializedImplementationOf(mbr)
          }
        }
      }

      // populate the specializedMembers data structure for the new members also
      for ((m, newMbr) <- newMembers if (m.isMethod && !m.isConstructor)) {
        if (specializedMembers(m).isDefinedAt(pspec)) {
          val newMbrMeantForSpec = newMembers(specializedMembers(m)(pspec))
          if (!(specializedMembers isDefinedAt newMbrMeantForSpec)) {
            specializedMembers(newMbrMeantForSpec) = new HashMap[PartialSpec, Symbol]
            for ((s, m) <- specializedMembers(m)) {
              specializedMembers(newMbrMeantForSpec)(s) = newMembers(m)
            }
          }
          specializedMembers(newMbr) = specializedMembers(newMbrMeantForSpec)
        } else
          // member not specialized:
          specializedMembers(newMbr) = HashMap.empty
      }

      // specialized specializedMembers
      addSpecialOverrides(pspec, localPspec, spec, specScope, inPlace = true)

      // deferred type tags:
      addDeferredTypeTagImpls(spec, specScope, inPlace = true)

      // normalized members:
      addNormalizedMembers(spec, specScope, inPlace = true)

      spec
    }

    def widen(specs: List[PartialSpec]): List[Symbol] = {

      origin setFlag(MINIBOXED)

      specializedStemClass(origin) = origin
      variantParamMap(origin) = origin.info.typeParams.map((p: Symbol) => (p, p)).toMap
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

      // we make specialized specializedMembers for every member of the original class
      for (member <- methods ::: getters ::: setters if !notSpecializable(origin, member)) {

        val specializedMembersOfMember = new HashMap[PartialSpec, Symbol]
        val specs_filtered =  if (needsSpecialization(origin, member)) specs else specs.filter(PartialSpec.isAllAnyRef(_))

        for (spec <- specs_filtered) {
          var newMbr = member
          if (!PartialSpec.isAllAnyRef(spec)) {
            val env: TypeEnv = spec map {
              case (p, v) => (p, if (v == Boxed) p.tpe else storageType(p))
            }
            val specializedTypeMap = MiniboxSubst(env)

            newMbr = member.cloneSymbol(origin)

            // https://github.com/miniboxing/miniboxing-plugin/issues/82
            // specialized parameter accessors are not accessors anymore
            // as they violate the premise that they do not take parameters
            if (newMbr.isParamAccessor)
              newMbr.resetFlag(PARAMACCESSOR) // we don't want this to be a PARAMACCESSOR

            newMbr setFlag MINIBOXED
            newMbr setName (specializedName(member.name, typeParamValues(origin, spec)))
            newMbr modifyInfo (info => {
              val info0 = specializedTypeMap(info.asSeenFrom(newMbr.owner.thisType, member.owner))
              val localTags =
                for (tparam <- origin.typeParams if tparam.hasFlag(MINIBOXED) && spec(tparam) == Miniboxed)
                  yield (newMbr.newValue(shortTypeTagName(tparam), newMbr.pos).setInfo(ByteClass.tpe), tparam)
              localTypeTags(newMbr) = localTags.toMap
              val tagParams = localTags.map(_._1)
              val info1 =
                info0 match {
                  case MethodType(args, ret) =>
                    MethodType(tagParams ::: args, ret)
                  case PolyType(targs, MethodType(args, ret)) =>
                    val ntargs = targs.map(_.cloneSymbol(newMbr))
                    val tpe = MethodType(tagParams ::: args, ret).substSym(targs, ntargs)
                    assert((tagParams ::: args).length == tpe.params.length, tagParams + ", " + args + ", " + tpe.params)
                    val update = ((tagParams ::: args) zip tpe.params).toMap
                    localTypeTags(newMbr) = localTypeTags(newMbr).map({ case (tag, t) => (update(tag), t) })
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
              if (specializedMembers.isDefinedAt(alias) && specializedMembers(alias).isDefinedAt(pspec2)) {
                newMbr.asInstanceOf[TermSymbol].referenced = specializedMembers(alias)(pspec2)
              } else {
                log("Could not rewire!")
                log("base: " + newMbr.defString)
                log("from: " + alias.defString)
                log(baseTpe)
                log(baseTpe.typeSymbol.typeParams)
                log(baseTpe.typeArgs)
                log(pspec2)
                log(specializedMembers.get(alias))
                log("")
              }
            }

            newMembers ::= newMbr
          }

          specializedMembersOfMember(spec) = newMbr
          specializedMembers(newMbr) = specializedMembersOfMember
          specializationStemMember(newMbr) = member
        }

        for (spec <- specs; newMbr <- specializedMembersOfMember get spec)
          // TODO TOPIC/ERASURE: Check this is correct, it may be wrong
          memberSpecializationInfo(newMbr) = genForwardingInfo(member)
      }

      // TODO: Do we want to keep overrides?
      origin.info.decls.toList ++ newMembers
    }

    // create overrides for specialized variants of the method
    def addSpecialOverrides(globalPSpec: PartialSpec, pspec: PartialSpec, clazz: Symbol, scope: Scope, inPlace: Boolean = false): Scope = {

      val scope1 = if (inPlace) scope else scope.cloneScope
      val base = specializedStemClass.getOrElse(clazz, NoSymbol)

      def specializedOverriddenMembers(sym: Symbol): Symbol = {
        for (baseOSym <- sym.allOverriddenSymbols) {

          // Check we're not incorrectly overriding normalized members:
          // class B           {          def foo[@miniboxed T, U] = ???            }
          // class C extends B { override def foo[@miniboxed T, @miniboxed U] = ??? } // OK
          // class C extends D { override def foo[@miniboxed T, U] = ??? }            // NOT OK
          if (sym.typeParams.nonEmpty) {
            val tparamMap = (baseOSym.typeParams zip sym.typeParams).toMap
            val tparamMiss = baseOSym.typeParams.filter(tparam =>
              tparam.isMiniboxAnnotated && !tparamMap(tparam).isMiniboxAnnotated).map(tparamMap)
            if (tparamMiss.nonEmpty)
              currentUnit.error(sym.pos, "The " + sym + " in " + clazz + " overrides " + baseOSym + " in " + baseOSym.owner + " therefore needs to have the follwing type parameters marked with @miniboxed: " + tparamMiss.mkString(",") + ".")
          }

          if (isSpecializableClass(baseOSym.owner) && base != baseOSym.owner) {
            // here we get the base class, not the specialized class
            // therefore, the 1st step is to identify the specialized class
            val baseParent = baseOSym.owner
            val baseParentTpe = clazz.info.baseType(baseParent)
            val spec = PartialSpec.fromTypeInContext(baseParentTpe.asInstanceOf[TypeRef], pspec)
            if (!PartialSpec.isAllAnyRef(spec) && specializedMembers.isDefinedAt(baseOSym)) {
              specializedMembers(baseOSym).get(spec) match {
                case Some(mainSym) =>
                  return mainSym
                case _ =>
              }
            }
          }
        }
        NoSymbol
      }

    if (clazz.isClass) // class or trait
      for (sym <- scope1 if sym.isMethod && !sym.isConstructor) {
        specializedOverriddenMembers(sym).toOption.foreach(oSym => {
          val localOverload = specializedMembers.get(sym).flatMap(_.get(globalPSpec)).getOrElse(NoSymbol)
          // only generate the override if we don't have an overload which matches the current symbol:
          // matching the symbol is a pain in the arse, since oSym points to the interface while localOverload
          // points to the current clazz -- TODO: we could carry newMembers and get the correspondence
          if (localOverload.name != oSym.name) {
            val overrider = oSym.cloneSymbol(clazz)
            overrider.setInfo(oSym.info.cloneInfo(overrider).asSeenFrom(clazz.thisType, oSym.owner))
            overrider.resetFlag(DEFERRED).setFlag(OVERRIDE)

            val paramUpdate = (oSym.info.params zip overrider.info.params).toMap
            val baseClazz = oSym.owner
            val baseType = clazz.info.baseType(baseClazz)
            val tparamUpdate = (baseClazz.typeParams zip baseType.typeArgs.map(_.typeSymbol)).toMap
            val typeTags = localTypeTags.getOrElse(oSym, Map.empty).map({ case (tag, tpe) => (paramUpdate(tag), tparamUpdate(tpe))})

            // copy the body to the specialized overload and let the symbol forward. There is no optimal solution
            // when using nested class specialization:
            // ```
            //   abstract class C[T, U] { def foo(t: T, u: U): Any }
            //   class X[Y] { new C[Int, Y] { def foo(t: Int, u: Y) = ??? }
            // ```
            // which method should carry the body in class X_J? foo or foo_JJ?
            //  * `foo`    gets t: Int unboxed and u: Y boxed
            //  * `foo_JJ` gets both t and u as miniboxed, which is still suboptimal, but better
            localTypeTags(overrider) = typeTags

            memberSpecializationInfo.get(sym) match {
              // if sym is a forwarder to a more specialized member, let the overrider forward to
              // the the most specialized member, else we're losing optimality
              case Some(ForwardTo(moreSpec)) =>
                memberSpecializationInfo(overrider) = genForwardingInfo(sym, overrider = true)

              // if sym is the most specialized version of the code, then just move it over to the
              // new overrider symbol, exactly like in the example above -- `foo_JJ`
              case _ =>
                memberSpecializationInfo(sym) = genForwardingInfo(sym, overrider = true)
                memberSpecializationInfo(overrider) = SpecializedImplementationOf(sym)
            }
            specializedMembers.getOrElseUpdate(sym, collection.mutable.HashMap()) += (pspec -> overrider)

            scope1 enter overrider
          }
        })
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
          impl.resetFlag(DEFERRED | ABSTRACT)
          memberSpecializationInfo(impl) = DeferredTypeTagImplementation(tparam)
          scope1 enter impl
        }
        inheritedDeferredTypeTags(origin).clear()
      }
      scope1
    }

    // normalize methods
    def addNormalizedMembers(origin: Symbol, scope: Scope, inPlace: Boolean = false): Scope = {
      val newScope = if (inPlace) scope else scope.cloneScope
      for (mbr <- scope.cloneScope)
        normalizeMembers(mbr).foreach(newScope.enter _)
      newScope
    }

    /*
     * This removes fields and constructors from a class while leaving the
     * setters and getters in place. The effect is that the class automatically
     * becomes an interface
     */
    private def adaptClassFields(clazz: Symbol, decls1: Scope): Scope = {
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

    // begin specialize

    // force info on parents to get all specialized metadata
    afterMiniboxDupl(originTpe.parents.map(_.typeSymbol.info))
    val specs = if (isSpecializableClass(origin)) specializations(origin.info.typeParams) else Nil
    specs.map(_.map(_._1.setFlag(MINIBOXED))) // TODO: Only needs to be done once per type parameter

    val tpe = if (specs.nonEmpty) {
      log("Specializing " + origin + "...\n")

      // step1: widen the class or trait
      val scope1 = newScopeWith(widen(specs): _*)

      // mark this symbol as the base of a miniboxed hierarchy
      specializedStem += origin
      variantTypeEnv(origin) = (origin.typeParams zip origin.typeParams.map(_.tpeHK)).toMap

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
        if (origin.owner.info.decls != EmptyScope)
          afterMiniboxDupl(origin.owner.info.decls enter spc)

        spc
      }

      // for traits resulting from classes inheriting each other we need to insert an artificial AnyRef parent
      val artificialAnyRefReq = !origin.isTrait && ((originTpe.parents.size >= 1) && (specializedStem(originTpe.parents.head.typeSymbol)))
      val artificialAnyRef = if (artificialAnyRefReq) List(AnyRefTpe) else Nil
      val parents1 = artificialAnyRef ::: originTpe.parents

      val scope2 = adaptClassFields(origin, scope1)
      val scope3 = addNormalizedMembers(origin, scope2)

      log("  // interface:")
      log("  " + origin.defString + " {")
      for (decl <- scope3.toList.sortBy(_.defString))
        log(f"    ${decl.defString}%-70s")

      log("  }\n")

      classes foreach { cls =>
        log("  // specialized class:")
        log("  " + cls.defString + " {")
        for (decl <- cls.info.decls.toList.sortBy(_.defString) if !dummyConstructors(decl))
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
      val scope4 = addNormalizedMembers(origin, scope3)
      // make all structural refinement members private (members may be added by special overrides and normalizedMembers)
      GenPolyType(origin.info.typeParams, ClassInfoType(parents1, scope4, origin))
    }

    for (mbr <- tpe.decls) {
      if (mbr.isStructuralRefinementMember) {
//        println(s"SETTING PROTECTED: ${mbr.defString} in $origin: ${mbr.isStructuralRefinementMember}")
        mbr.setFlag(PROTECTED)
      }
    }

    tpe
  }

  /**
   * Generate the information about how arguments and return value should
   * be converted when forwarding to `target`.
   */
  private def genForwardingInfo(base: Symbol, overrider: Boolean = false): ForwardTo = {
    ForwardTo(base)(overrider)
  }
}
