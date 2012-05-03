package plugin

import scala.reflect.internal.Flags

trait MiniboxTransformation {
  self: MiniboxLogic with MiniboxLogging => 

    import global._
    import definitions._
    import Flags._
    lazy val MinispecedClass = definitions.getRequiredClass("plugin.minispec")

    /**
     * This function performs the info transformation
     */
    def miniboxTransformInfo(sym: Symbol, tpe: Type) = 
      if (sym hasAnnotation MinispecedClass) {
        // tpe will always be PolyType(tArgs, ClassInfoType(parents, decls, typeSym))
        // TODO: What if the type parameters are given in the outside class?
        tpe.resultType match {
          case ClassInfoType(parents, decls, typeSym) =>
            // Steps we need to do:
            // Step 1 -- transform the generic class
            
            // Step 2 -- for each of the specializations, forward the members to do the right thing
            
            
            // 1. Decide on the specializations
            val envs = specializations(tpe.typeParams)
            widenClass(sym, tpe, envs)
            // 2. Perform actual specialization
            val classes = envs.map((env: TypeEnv) => specializeClass(sym, tpe, env))
//            for ((spec, clazz) <- (envs zip classes))
//              println("\n\nSPECIALIZED FOR " + spec + " => \n" + clazz + ": " + clazz.info)
            tpe
          case _ =>
            println("Not specializing: " + sym)
            tpe
        }
      } else tpe

    def miniboxTransformTree(tree: Tree): Tree = { 
      //    val x = 1
      //    class MiniboxPhase(prev: Phase) extends StdPhase(prev) {
      //      override def name = Minibox.this.name
      //      def apply(unit: CompilationUnit) {
      //        for ( tree @ Apply(Select(rcvr, nme.DIV), List(Literal(Constant(0)))) <- unit.body;
      //             if rcvr.tpe <:< definitions.IntClass.tpe) 
      //          {
      //            unit.error(tree.pos, "definitely division by zero")
      //          }
      //      }
      //    }
      println("tree: " + tree)
      tree
    }
    
    /**
     * Add the necessary methods to the class such that access can be done unboxed
     */
    def widenClass(clazz: Symbol, tpe: Type, envs: List[TypeEnv]) = {
      /* We have:
       * class C[T] extends Object {
       *   private[this] val a: T
       *   def <init>(a: T): C[T]
       *   val a: T
       *   def foo(x: T): T
       * }
       * 
       * we want
       *
       * class C[T](a: T) /* the one that extends object */ {
       *   private[this] var a: T
       *   def <init>(a: T)
       *   def a: T
       *   def aO: T
       *   def aN: T
       *   def foo(x: T): T
       *   def fooO(x: T): T
       *   def fooN(x: T): T
       *   def specInstance: Int
       * }
       * 
       */
      // we only specialize the members that are defined in the current class
      val members = clazz.info.members.filter(_.owner == clazz)
      println("all: \n" +     members.map(" * " + _.defString + "\n").mkString)
      val methods = members.filter(s => s.isMethod && !(s.isConstructor || s.isGetter || s.isSetter))
      val ctors   = members.filter(_.isConstructor)
      val getters = members.filter(_.isGetter)
      val setters = members.filter(_.isSetter)
      println("ctors: \n" +   ctors.map(" * " + _.defString + "\n").mkString)
      println("methods: \n" + methods.map(" * " + _.defString + "\n").mkString)
      println("getters: \n" + getters.map(" * " + _.defString + "\n").mkString)
      println("setters: \n" + setters.map(" * " + _.defString + "\n").mkString)

      // we leave the fields untouched, only touching the methods
      var newMembers = List[Symbol]()
      
      for (member <- methods ::: getters ::: setters)
        if (needsSpecialization(clazz, member))
          for (env <- envs if !isAllAnyRef(env)) {
            val newMbr = member.cloneSymbol(clazz).setName(specializedName(member.name, typeParamValues(clazz, env)))
            // TODO: Change type here
            newMbr.info = member.info
            clazz.info.resultType.decls enter(newMbr)
          }
      
      println(clazz.info)
      
      clazz
    }
    
    
    /**
     * Actual class specialization. Steps required:
     *  1. Create the transformed set of functions
     *  2. For each of the specializations, decide what to do with each function
     *  3. Create the actual signatures
     */
    def specializeClass(clazz: Symbol, tpe: Type, env: TypeEnv): Symbol = {
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
      val sClassName = specializedName(clazz.name, clazz.typeParams.map(env)).toTypeName
      val sClass = clazz.owner.newClass(sClassName, clazz.pos, clazz.flags & ~CASE)
      
      sClass.setInfo(clazz.info)

//      def cloneInSpecializedClass(member: Symbol, flagFn: Long => Long, newName: Name = null) =
//        member.cloneSymbol(sClass, flagFn(member.flags | SPECIALIZED), newName)
//
//      sClass.sourceFile = clazz.sourceFile
//      currentRun.symSource(sClass) = clazz.sourceFile // needed later on by mixin
      sClass
    }
}