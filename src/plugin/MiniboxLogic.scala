package plugin

import scala.tools.nsc.Global

trait MiniboxLogic {
  self: MiniboxLogging =>

  // we need it, there's no way around
  val global: Global
  import global._
  import definitions._
  import scala.collection.immutable

  lazy val MinispecedClass = definitions.getRequiredClass("plugin.minispec")

  // TypeEnv maps type parameters to types
  type TypeEnv = immutable.Map[Symbol, Type]

  /**
   * For a set of type parameters, get all the specialized environments
   * TODO: We'll need to refine this in order to accommodate miniboxing + real specialization. This might include adding
   * a special 'miniboxed' type which acts as a list of types that are treated under miniboxing
   */
  def specializations(tParams: List[Symbol]): List[TypeEnv] = {
    var envs: List[List[Type]] = List(Nil)

    for (tParam <- tParams)
      envs = envs.flatMap(rest => List(LongClass.tpe :: rest, AnyRefClass.tpe :: rest))

    envs.map((types: List[Type]) => (tParams zip types).toMap)
  }

  /**
   * Creates the name of the interface which corresponds to class `className`
   */
  def interfaceName(className: Name): TypeName = {
    newTypeName(className.toString + "_interface")
  }

  /**
   * Specialize name for the two list of types.
   */
  def specializedName(name: Name, types: List[Type]): TermName = {
    if (nme.INITIALIZER == name || (types.isEmpty))
      name
    else if (nme.isSetterName(name))
      nme.getterToSetter(specializedName(nme.setterToGetter(name), types))
    else if (nme.isLocalName(name))
      nme.getterToLocal(specializedName(nme.localToGetter(name), types))
    else {
      newTermName(name.toString + "_" + types.map(t => definitions.abbrvTag(t.typeSymbol)).mkString(""))
    }
  }

  /**
   * Find the variable that should be accessed by some specialized accessor
   */
  def accessed(m: Symbol): Symbol = {
    val getterName = m.getterName
    val originalGetterName = if (getterName.containsChar('_'))
      getterName.subName(0, getterName.indexOf('_'))
    else
      getterName

    m.owner.info.decl(nme.getterToLocal(originalGetterName))
  }

  def typeParamValues(clazz: Symbol, env: TypeEnv) = clazz.typeParams.map(env)

  def needsSpecialization(clazz: Symbol, method: Symbol) = true

  def isAllAnyRef(env: TypeEnv) = env.forall(_._2 == AnyRefClass.tpe)

}