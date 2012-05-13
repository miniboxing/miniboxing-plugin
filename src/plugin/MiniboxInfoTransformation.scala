package plugin

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.InfoTransform
import scala.collection.mutable.HashMap

trait MiniboxInfoTransformation extends InfoTransform {
  self: MiniboxPhase with MiniboxLogic with MiniboxLogging with MiniboxSpecializationInfo =>

  import global._
  import Flags._
  import definitions._

  /**
   * For each method of the original class and each type environment we keep track
   * of the overload specialized for that environment.
   */
  val overloads = new HashMap[Symbol, HashMap[PartialSpec, Symbol]]

  /**
   * The environment for which a method is "meant"
   */
  val envOf = new HashMap[Symbol, PartialSpec]

  /**
   * This function performs the info transformation
   */
  override def transformInfo(sym: Symbol, tpe: Type): Type =
    // XXX: even for non specialized classes we need to add `special overrides`
    if (sym.typeParams exists (_ hasAnnotation MinispecedClass)) {
      println("Specializing " + sym + "...")
      tpe match {
        case PolyType(tArgs, ClassInfoType(parents, decls, typeSym)) =>
          val clazz = sym
          // Create a list of specializations that we are going to create
          val envs = specializations(tpe.typeParams)

          // For each method in the original class, add its specialized overloads
          widenClass(clazz, envs)
          println("-------------- WIDENDED CLAZZ ----------------")
          println(clazz.name + " " + clazz.info);

          // Build the interface to be implemented by all specialized classes
          val iface = createGenericInterface(clazz)
          println("-------------- INTERFACE ----------------")
          println(iface.name + " " + iface.info);

          val ifaceParentType = iface.tpe.instantiateTypeParams(iface.typeParams, clazz.typeParams map (_.tpe))
          clazz setInfo PolyType(tArgs, ClassInfoType(parents ::: List(ifaceParentType), decls, typeSym))
          println("-------------- ORIGINAL CLASS ----------------")
          println(clazz.name + " " + clazz.info);

          // 4. Perform actual specialization
          val classes = envs map (specializeClass(clazz, iface.tpe, _))

          classes foreach { cls =>
            println("------------ SPEC CLASS ------------")
            println(cls.name + " " + cls.info)
          }

          println("-------------- TEMPLATE MEMBERS ----------------")
          templateMembers foreach (m => println(m.fullName))
          //          for ((m, info) <- memberSpecializationInfo) 
          //            println("%s - %s".format(m.fullName, info))

          tpe
        case _ =>
          println("Not specializing: " + sym)
          tpe
      }
    } else tpe

  /**
   * For each method in the original class and each possible type parameter
   * assignment, we generate an overload that "assumes" that the class is
   * instantiated with that combination of type arguments.
   *
   * Now the original class becomes a template for creating the interface and the
   * specialized versions.
   */
  def widenClass(clazz: Symbol, specs: List[PartialSpec]) = {
    // we only specialize the members that are defined in the current class
    val members = clazz.info.members.filter(_.owner == clazz)

    val methods = members.filter(s => s.isMethod && !(s.isConstructor || s.isGetter || s.isSetter))
    val ctors = members.filter(_.isConstructor)
    val getters = members.filter(_.isGetter)
    val setters = members.filter(_.isSetter)
    val fields = members.filter(m => m.isTerm && !m.isMethod)

    // we leave the fields untouched, only touching the methods
    for (member <- methods ::: getters ::: setters if (needsSpecialization(clazz, member))) {
      val overloadsOfMember = new HashMap[PartialSpec, Symbol]
      for (spec <- specs) {
        var newMbr = member
        if (!isAllAnyRef(spec)) {
          val env: TypeEnv = spec map {
            case (p, v) => (p, if (v == Boxed) p.tpe else LongClass.tpe)
          }

          newMbr = member.cloneSymbol(clazz)

          newMbr setFlag SPECIALIZED
          newMbr setName (specializedName(member.name, typeParamValues(clazz, spec)))
          newMbr modifyInfo (info => subst(env, info.asSeenFrom(newMbr.owner.thisType, newMbr.owner)))

          memberSpecializationInfo(newMbr) = genForwardingInfo(clazz, newMbr, member)

          clazz.info.resultType.decls enter (newMbr)
        }

        overloadsOfMember(spec) = newMbr
        overloads(newMbr) = overloadsOfMember
        envOf(newMbr) = spec
      }
    }
  }

  /*
   * Substitute the type parameters with their value as given by the 'env'
   * in the type 'tpe'. 
   */
  private def subst(env: TypeEnv, tpe: Type): Type = {
    val (keys, values) = env.toList.unzip
    (new SubstTypeMap(keys, values))(tpe)
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
   * Creates the generic interface of the `clazz`. It contains the
   * methods of `clazz` with all their specialized overloads.
   */
  private def createGenericInterface(clazz: Symbol): Symbol = {
    val sClassName = interfaceName(clazz.name)
    val iface: Symbol =
      clazz.owner.newClass(sClassName, clazz.pos, clazz.flags | INTERFACE | TRAIT | ABSTRACT)

    // Copy the methods into the interface and replace the type parameters with fresh ones
    val pmap = ParamMap(clazz.typeParams, iface)
    specializedInterface(clazz) = iface

    val ifaceDecls = newScope
    for (decl <- clazz.info.decls if decl.isMethod && !decl.isConstructor) {
      val d = decl.cloneSymbol(iface, decl.flags | SPECIALIZED) modifyInfo substParams(pmap)
      // record the fact that the method `d` will not have an implementation
      memberSpecializationInfo(d) = Interface()
      ifaceDecls enter d
    }
    val interfaceType =
      PolyType(pmap.values.toList, ClassInfoType(List(AnyRefClass.tpe), ifaceDecls, iface))

    iface setInfo interfaceType

    iface
  }

  /**
   * Actual class specialization. `clazz` is the template, `iface` is the interface
   * to be extended and `env` is the type parameter mapping for which we specialize.
   */
  def specializeClass(clazz: Symbol, iface: Type, spec: PartialSpec): Symbol = {
    val sClassName = specializedName(clazz.name, typeParamValues(clazz, spec)).toTypeName
    val sClass = clazz.owner.newClass(sClassName, clazz.pos, clazz.flags)

    sClass.sourceFile = clazz.sourceFile
    currentRun.symSource(sClass) = clazz.sourceFile

    /*
     * All the info that we copy uses the type parameters of the original class
     * `pmap` allows us to change them to the new parameters, while `specEnv` have
     * mapping to their or to the actual value class (if any).
     */
    val pmap = ParamMap(clazz.typeParams, sClass)
    val env: TypeEnv = spec map {
      case (p, Miniboxed) => (p, LongClass.tpe)
      case (p, Boxed) => (p, pmap(p).tpe)
    }

    // insert the new symbol in the info maps
    specializedClasses(clazz) ::= sClass
    typeEnv(sClass) = env
    partialSpec(sClass) = spec

    // declarations inside the specialized class - to be filled in later
    val sClassDecls = newScope

    // create the type of the new class
    val specializedInfoType: Type = {
      // TODO: replace parents with the specialized version
      val sParents = (clazz.info.parents) map {
        t => (subst(env, t))
      }

      // parameters which are not fixed
      val newTParams: List[Symbol] =
        (env.values map (_.typeSymbol) filterNot isPrimitiveValueClass).toList
      GenPolyType(newTParams, ClassInfoType(sParents, sClassDecls, sClass))
    }
    sClass setInfo specializedInfoType // ??? afterMinibox ???

    /*
     * Add type tag fields to the new class and keep a mapping between type 
     * parameters and the corresponding types tags. 
     * 
     * TODO: Meanwhile, erase the `evidence`: Manifest[T] field.
     */

    val typeTagMap: Map[Symbol, Symbol] =
      (for ((tparam, tval) <- env if tval == LongClass.tpe) yield {
        val accFlags = SYNTHETIC | PARAMACCESSOR | PrivateLocal
        val sym = sClass.newValue(typeTagName(tparam), sClass.pos, accFlags)
        sym setInfo ByteClass.tpe
        sym setFlag SPECIALIZED

        sClassDecls enter sym
        (tparam, sym)
      }).toMap

    typeTags(sClass) = typeTagMap

    /*
     * Copy the members from the original class `clazz` to the specialized class
     * `sClass`, and use the `newTParams` symbols in their definitions. Also, 
     * replace the this type of `clazz` with that of `sClass`.
     * 
     * For fields, use the environment to determine their type.
     * 
     * For each newly introduced symbol, record how its tree should be generated.
     */
    val newMembers = (for (m <- clazz.info.members if m.owner == clazz) yield (m, m.cloneSymbol(sClass))).toMap

    for ((m, newMbr) <- newMembers; envOfm <- envOf get m)
      envOf(newMbr) = envOfm

    for ((m, newMbr) <- newMembers) {
      newMbr setFlag SPECIALIZED
      newMbr modifyInfo { info =>
        val info1 = info.substThis(clazz, sClass)
        if (m.isConstructor) { // constructor - add type tags as parameters
          val tagParams = typeTagMap.values map (_.cloneSymbol(newMbr))
          MethodType(subst(env, info1).params ++ tagParams, newMbr.owner.tpe)
        } else if (m.isTerm && !m.isMethod && m.isImplicit) { // implicit fields
          info1
        } else {
          subst(env, info1)
        }
      }
      sClassDecls enter newMbr
    }

    for ((m, newMbr) <- newMembers) {
      if (m.isConstructor) { // constructor
        memberSpecializationInfo(newMbr) = SpecializedImplementationOf(m)
      } else if (m.isTerm && !m.isMethod) { // fields
        memberSpecializationInfo(newMbr) = SpecializedImplementationOf(m)
      } else {
        if (m.isDeferred)
          memberSpecializationInfo(newMbr) = Interface()

        /* Check whether the method is the one that will carry the
         * implementation. If yes, find the original method from the original
         * class from which to copy the implementation. If no, find the method 
         * that will have an implementation and forward to it.
         */
        if (overloads(m)(spec) == m) {
          if (m.hasAccessorFlag) {
            memberSpecializationInfo(newMbr) = FieldAccessor(newMembers(accessed(m)))
          } else {
            memberSpecializationInfo.get(m) match {
              case info: Some[_] => info.get match {
                case ForwardTo(original, _, _) =>
                  memberSpecializationInfo(newMbr) = SpecializedImplementationOf(original)
              }
              case None =>
                memberSpecializationInfo(newMbr) = SpecializedImplementationOf(m)
            }
          }
        } else {
          val target = newMembers(overloads(m)(spec))
          memberSpecializationInfo(newMbr) = genForwardingInfo(sClass, newMbr, target)
        }
      }
    }

    sClass
  }

  /**
   * Generate the information about how arguments and return value should
   * be converted when forwarding to `target`.
   *
   * XXX: need to generate tags for all type params. not only for specialized
   * ones.
   */
  private def genForwardingInfo(sClass: Symbol, wrapper: Symbol, target: Symbol): ForwardTo = {
    //    val wenv = envOf(wrapper)
    //    val tenv = envOf(target)

    def genCastInfo(srcType: Type, tgtType: Type): CastInfo = {
      if (srcType == LongClass.tpe && tgtType != LongClass.tpe) {
        CastMiniboxToBox(null)
      } else if (srcType != LongClass.tpe && tgtType == LongClass.tpe) {
        CastBoxToMinibox
      } else if (srcType == tgtType) {
        NoCast
      } else {
        sys.error("A cast which is neither boxing, nor unboxing when handling `ForwardTo`." +
          srcType + " --> " + tgtType)
      }
      //      (wenv get srcType.typeSymbol, tenv get tgtType.typeSymbol) match {
      //        case (Some(wptpe), Some(tptpe)) if (wptpe == LongClass.tpe && tptpe != LongClass.tpe) =>
      //          CastMiniboxToBox(typeTags(sClass)(tgtType.typeSymbol))
      //        case (Some(wptpe), Some(tptpe)) if (wptpe != LongClass.tpe && tptpe == LongClass.tpe) =>
      //          CastBoxToMinibox
      //        case _ => NoCast
      //      }
    }

    val paramCasts = (wrapper.tpe.params zip target.tpe.params) map {
      case (wtp, ttp) =>
        genCastInfo(wtp.tpe, ttp.tpe)
    }
    val retCast = genCastInfo(target.tpe.resultType, wrapper.tpe.resultType)
    ForwardTo(target, retCast, paramCasts)
  }

}


