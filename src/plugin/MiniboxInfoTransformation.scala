package plugin

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.InfoTransform

trait MiniboxInfoTransformation extends InfoTransform {
  self: MiniboxPhase with MiniboxLogic with MiniboxLogging with MiniboxSpecializationInfo =>

  import global._
  import Flags._

  /**
   * This function performs the info transformation
   */
  override def transformInfo(sym: Symbol, tpe: Type): Type =
    if (sym.typeParams exists (_ hasAnnotation MinispecedClass)) {
      tpe match {
        case PolyType(tArgs, ClassInfoType(parents, decls, typeSym)) =>
          val clazz = sym
          // Create a list of specializations that we are going to create
          val envs = specializations(tpe.typeParams)

          // For each method in the original class, add its specialized overloads
          widenClass(clazz, envs)
          println("-------------- WIDENDED CLAZZ ----------------")
          println(clazz.info);

          // Build the interface to be implemented by all specialized classes
          val iface = createGenericInterface(clazz)
          println("-------------- INTERFACE ----------------")
          println(iface.info);

          // 4. Perform actual specialization
          val classes = envs map (specializeClass(clazz, iface.tpe, _))

          // TODO: make `clazz` extend the version specialized with object
          //clazz setInfo PolyType(tArgs, ClassInfoType(List(allAnyRefClass(clazz).tpe), newScope, typeSym))

          tpe
        case _ =>
          println("Not specializing: " + sym)
          tpe
      }
    } else tpe

  /**
   * Add the specialized overloads to the original class
   */
  def widenClass(clazz: Symbol, envs: List[TypeEnv]) = {

    // we only specialize the members that are defined in the current class
    val members = clazz.info.members.filter(_.owner == clazz)
    //    println("all: \n" + members.map(" * " + _.defString + "\n").mkString)
    val methods = members.filter(s => s.isMethod && !(s.isConstructor || s.isGetter || s.isSetter))
    val ctors = members.filter(_.isConstructor)
    val getters = members.filter(_.isGetter)
    val setters = members.filter(_.isSetter)
    val fields = members.filter(m => m.isTerm && !m.isMethod)

//    println("ctors: \n" + ctors.map(" * " + _.defString + "\n").mkString)
//    println("methods: \n" + methods.map(" * " + _.defString + "\n").mkString)
//    println("getters: \n" + getters.map(" * " + _.defString + "\n").mkString)
//    println("setters: \n" + setters.map(" * " + _.defString + "\n").mkString)
//    println("fields: \n" + fields.map(" * " + _.defString + "\n").mkString)

    // we leave the fields untouched, only touching the methods
    for (member <- methods ::: getters ::: setters)
      if (needsSpecialization(clazz, member))
        for (env <- envs if !isAllAnyRef(env)) {
          val newMbr = member.cloneSymbol(clazz)

          newMbr setFlag SPECIALIZED
          newMbr setName (specializedName(member.name, typeParamValues(clazz, env)))
          newMbr modifyInfo (info => subst(env, info.asSeenFrom(newMbr.owner.thisType, newMbr.owner)))

          clazz.info.resultType.decls enter (newMbr)

          methodSpecializationInfo(newMbr) = ForwardTo(member)
        }

    clazz
  }

  /*
   *  Substitute the type parameters with their value as given by the 'env'
   *  in the type 'tpe'. 
   */
  private def subst(env: TypeEnv, tpe: Type): Type = {
    val (keys, values) = env.toList.unzip
    (new SubstTypeMap(keys, values))(tpe)
  }
  private def substParams(pmap: ParamMap): Type => Type = {
    val (keys, values) = pmap.toList.unzip
    (new SubstTypeMap(keys, values map (_.tpe)))
  }

  /*
   * Creates the generic interface of the `clazz`. It contains the
   * methods of `clazz` with all their specialized overloads.
   */
  private def createGenericInterface(clazz: Symbol): Symbol = {
    val sClassName = interfaceName(clazz.name)
    val iface: Symbol = clazz.owner.newClass(sClassName, clazz.pos, clazz.flags | INTERFACE)

    // Copy the methods into the interface and replace the type parameters with new ones
    val pmap = ParamMap(clazz.typeParams, iface)
    specializedInterface(clazz) = ClassInfo(iface, pmap)

    val ifaceDecls = newScope
    for (decl <- clazz.info.decls if decl.isMethod && !decl.isConstructor) {
      ifaceDecls enter (decl.cloneSymbol(iface) modifyInfo substParams(pmap))
    }

    val interfaceType = PolyType(pmap.values.toList, ClassInfoType(Nil, ifaceDecls, iface))

    afterMinibox(iface setInfo interfaceType)
    iface
  }

  /* Need to specialize the class following the pattern:
   * 
   * class C[@minispec T](val a: T) {
   *   def foo(x: T): T = x
   * }
   * 
   * which is actually:
   * 
   * class C[T] extends Object {
   *   private[this] val a: T
   *   def <init>(a: T): C[T]
   *   val a: T
   *   def foo(x: T): T
   * }
   * 
   * into:
   *   class C[T](a: T) /* the one that extends object */ {
   *     private[this] var a: T
   *     def <init>(a: T)
   *     def aO: T
   *     def aN: T
   *     def fooO(x: T): T
   *     def fooN(x: T): T
   *     def specInstance: Int
   *   }
   * and
   *   class C_N[T] extends C[T] {
   *     private[this] var a: Long /* maybe a mangled name? */
   *     def <init>(T$type: Int, a: Long)
   *     def aO: T
   *     def aN: Long
   *     def fooO(x: T): T
   *     def fooN(x: Long): Long
   *     def specInstance: Int
   *   }
   *
   * TODO: How to do about the constructor(s)?
   */
  /**
   * Actual class specialization. Steps required:
   *  1. Create the transformed set of functions
   *  2. For each of the specializations, decide what to do with each function
   *  3. Create the actual signatures
   */
  def specializeClass(clazz: Symbol, iface: Type, env: TypeEnv): Symbol = {
    val sClassName = specializedName(clazz.name, clazz.typeParams.map(env)).toTypeName
    val sClass = clazz.owner.newClass(sClassName, clazz.pos, clazz.flags | SPECIALIZED)

    sClass.sourceFile = clazz.sourceFile
    currentRun.symSource(sClass) = clazz.sourceFile

    // insert the new symbol in the info maps
    val pmap = ParamMap(clazz.typeParams, sClass)
    specializedClasses(clazz)(env) = ClassInfo(sClass, pmap)
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
        subst(env, _).instantiateTypeParams(oldTParams, newTParams map (_.tpe))
      }

      GenPolyType(newTParams, ClassInfoType(sParents, sClassDecls, sClass))
    }

    
    
    // 2. add specialized members to the class and record what should be done with each of them
    for (m <- clazz.info.members if m.isTerm && m.owner == clazz) {
      val newMbr = m.cloneSymbol(sClass)
      
      if (m.isConstructor) {
        newMbr modifyInfo (info => MethodType(subst(env, info).params, m.owner.tpe)) 
        // XXX: (newMbr.owner.tpe) throwsNPE the type is replaced only after this phase
      } 
      
      if (m.isTerm && !m.isImplicit) { // field
        // XXX: do we want to replace with AnyRef, or with T for partial specializations?
        newMbr modifyInfo (t => substParams(pmap)(subst(env, t)))
      }

      sClassDecls enter newMbr
    }

    
    println("------------ SPEC CLASS ------------")
    println(env)
    println(specializedInfoType)
    afterMinibox(sClass setInfo specializedInfoType)
    sClass
  }
}

