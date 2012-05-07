package plugin

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.InfoTransform
import scala.collection.mutable.HashMap

trait MiniboxInfoTransformation extends InfoTransform {
  self: MiniboxPhase with MiniboxLogic with MiniboxLogging with MiniboxSpecializationInfo =>

  import global._
  import Flags._

  /**
   * For each method of the original class and each type environment we keep track
   * of the overload specialized for that environment. 
   */
  val overloads = new HashMap[Symbol, HashMap[TypeEnv, Symbol]]

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

          // 4. Perform actual specialization
          val classes = envs map (specializeClass(clazz, iface.tpe, _))

          classes foreach { cls =>
            println("------------ SPEC CLASS ------------")
            println(cls.name + " " + cls.info)
          }

          // TODO: make `clazz` extend the version specialized with object
          clazz setInfo PolyType(tArgs, ClassInfoType(parents ::: List(iface.tpe), decls, typeSym))
          println("-------------- ORIGINAL CLASS ----------------")
          println(clazz.name + " " + clazz.info);

          tpe
        case _ =>
          println("Not specializing: " + sym)
          tpe
      }
    } else tpe

  /**
   * Make the original class into a template for creating the interface and the
   * specialized versions.
   */
  def widenClass(clazz: Symbol, envs: List[TypeEnv]) = {
    // we only specialize the members that are defined in the current class
    val members = clazz.info.members.filter(_.owner == clazz)

    val methods = members.filter(s => s.isMethod && !(s.isConstructor || s.isGetter || s.isSetter))
    val ctors = members.filter(_.isConstructor)
    val getters = members.filter(_.isGetter)
    val setters = members.filter(_.isSetter)
    val fields = members.filter(m => m.isTerm && !m.isMethod)

    // XXX: safer without: overloads.clear
    // we leave the fields untouched, only touching the methods
    for (member <- methods ::: getters ::: setters if (needsSpecialization(clazz, member))) {
      val overloadsOfMember = new HashMap[TypeEnv, Symbol]
      for (env <- envs) {
        var newMbr = member
        if (!isAllAnyRef(env)) {
          newMbr = member.cloneSymbol(clazz)

          
          newMbr setFlag SPECIALIZED
          newMbr setName (specializedName(member.name, typeParamValues(clazz, env)))
          newMbr modifyInfo (info => subst(env, info.asSeenFrom(newMbr.owner.thisType, newMbr.owner)))

          clazz.info.resultType.decls enter (newMbr)
        }

        overloadsOfMember(env) = newMbr
        overloads(newMbr) = overloadsOfMember
      }
    }
  }

  /*
   *  Substitute the type parameters with their value as given by the 'env'
   *  in the type 'tpe'. 
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
    val iface: Symbol = clazz.owner.newClass(sClassName, clazz.pos, clazz.flags | INTERFACE | TRAIT)

    // Copy the methods into the interface and replace the type parameters with new ones
    val pmap = ParamMap(clazz.typeParams, iface)
    specializedInterface(clazz) = ClassInfo(iface, pmap)

    val ifaceDecls = newScope
    for (decl <- clazz.info.decls if decl.isMethod && !decl.isConstructor) {
      val d = decl.cloneSymbol(iface) modifyInfo substParams(pmap)
      ifaceDecls enter d

      // record the fact that the method `d` will not have an implementation
      methodSpecializationInfo(d) = Interface()
    }

    val interfaceType = PolyType(pmap.values.toList, ClassInfoType(Nil, ifaceDecls, iface))

    iface setInfo interfaceType
    iface
  }

  /**
   * Actual class specialization. `clazz` is the template, `iface` is the interface
   * to be extended and `env` is the type parameter mapping for which we specialize.
   */
  def specializeClass(clazz: Symbol, iface: Type, env: TypeEnv): Symbol = {
    val sClassName = specializedName(clazz.name, clazz.typeParams.map(env)).toTypeName
    val sClass = clazz.owner.newClass(sClassName, clazz.pos, clazz.flags | SPECIALIZED)

    sClass.sourceFile = clazz.sourceFile
    currentRun.symSource(sClass) = clazz.sourceFile

    // insert the new symbol in the info maps
    val pmap = ParamMap(clazz.typeParams, sClass)
    specializedClasses(clazz)(null) = ClassInfo(sClass, pmap) // XXX
    if (isAllAnyRef(env)) {
      allAnyRefClass(clazz) = ClassInfo(sClass, pmap)
    }

    // declarations inside the specialized class - to be filled in later
    val sClassDecls = newScope
    val (oldTParams, newTParams) = pmap.toList.unzip

    // create the type of the new class
    val specializedInfoType: Type = {
      // FIXME: replace parents with the specialized version
      val sParents = (clazz.info.parents ::: List(iface)) map {
        t => substParams(pmap)(subst(env, t))
      }
      GenPolyType(newTParams, ClassInfoType(sParents, sClassDecls, sClass))
    }
    sClass setInfo specializedInfoType // ??? afterMinibox ???

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
    //for ((m,n) <- newMembers) println(m + " " + n)
    for ((m, newMbr) <- newMembers) {
      newMbr modifyInfo { info =>
        val info1 = info.substThis(clazz, sClass)

        val info2 =
          if (m.isConstructor) { // constructor
            methodSpecializationInfo(newMbr) = SpecializedImplementationOf(m)

            MethodType(subst(env, info1).params, newMbr.owner.tpe)
          } else if (m.isTerm && !m.isMethod) { // fields, except implicit definitions
            methodSpecializationInfo(newMbr) = SpecializedImplementationOf(m)

            if (m.isImplicit) info1 else subst(env, info1)
          } else {
            //println("-"+m.defString+"-")
            //println(" * " + overloads(m)(env))
     
            if (m.hasAccessorFlag) {
              methodSpecializationInfo(newMbr) = FieldAccessor(newMembers(accessed(m)))
            } else if (m.isDeferred) {
              methodSpecializationInfo(newMbr) = Interface()
            } else if (overloads(m)(env) == m) {
              methodSpecializationInfo(newMbr) = SpecializedImplementationOf(m)
            } else {
              val target = newMembers(overloads(m)(env))
              methodSpecializationInfo(newMbr) = ForwardTo(target)
            }

            info1
          }

        substParams(pmap)(info2)
      }

      sClassDecls enter newMbr
    }

    sClass
  }
}

