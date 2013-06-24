package miniboxing.plugin

import scala.tools.nsc.Global

trait MiniboxLogic {
  self: MiniboxComponent =>

  val global: Global
  import global._
  import definitions._
  import scala.collection.immutable

  lazy val MinispecedClass = rootMirror.getRequiredClass("miniboxing.plugin.minispec")

  /**
   * A `TypeEnv` maps each type parameter of the original class to the
   * actual type used in the specialized version to which this environment
   * correspond. This type may be either `Long` or a fresh type parameter
   *  `Tsp`.
   */
  type TypeEnv = immutable.Map[Symbol, Type]
  val EmptyTypeEnv: TypeEnv = Map.empty

  /**
   * A `PartialSpec` provides us information about the representation used
   * for values of a type parameter: either `Boxed` (as AnyRef) or
   * `Miniboxed` (as Long).
   */
  sealed trait SpecInfo
  case object Miniboxed extends SpecInfo
  case object Boxed extends SpecInfo
  type PartialSpec = immutable.Map[Symbol, SpecInfo]

  /**
   * For a set of type parameters, get all the possible partial specializations.
   *
   * A partial specialization is represented as a list that gives the types that
   * specifies what a type parameter is instantiated to.
   */
  def specializations(tParams: List[Symbol]): List[PartialSpec] = {
    var envs: List[List[SpecInfo]] = List(Nil)

    for (tParam <- tParams)
      envs = envs.flatMap(rest => List(Miniboxed :: rest, Boxed :: rest))

    envs.map((types: List[SpecInfo]) => (tParams zip types).toMap)
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
   * The name of the field carrying the type tag of corresponding to a type
   * parameter `tparam`
   */
  def typeTagName(tparam: Symbol): TermName = {
    tparam.name.append("_TypeTag").toTermName
  }
  def isTypeTagField(field: Symbol): Boolean = {
    field.name.endsWith("_TypeTag")
  }

  def typeParamValues(clazz: Symbol, env: PartialSpec): List[Type] =
    clazz.typeParams.map(env) map {
      case Boxed => AnyRefClass.tpe
      case Miniboxed => LongClass.tpe
    }

  def needsSpecialization(clazz: Symbol, method: Symbol) = true

  def isAllAnyRef(env: PartialSpec) = !env.isEmpty && env.forall(_._2 == Boxed)

  /**
   * Tells whether a class must be specialized by looking at the annotations
   */
  def isSpecializableClass(clazz: Symbol) =
    clazz.isClass &&
    !clazz.typeParams.isEmpty &&
    (clazz.typeParams forall (_ hasAnnotation MinispecedClass))

  final val MINIBOXED = 1L << 46 // we define our own flag

  /** TODO: Document this */
  case class MiniboxingTypeEnv(shallowEnv: TypeEnv, deepEnv: TypeEnv)

  object PartialSpec {
    def fromType(tpe: TypeRef): PartialSpec = tpe match {
      case TypeRef(pre, sym, args) =>
        val tparams = afterMinibox(sym.info).typeParams
        map2(tparams, args) {
          case (p, `UnitTpe`)    => (p, Miniboxed)
          case (p, `BooleanTpe`) => (p, Miniboxed)
          case (p, `ByteTpe`)    => (p, Miniboxed)
          case (p, `ShortTpe`)   => (p, Miniboxed)
          case (p, `CharTpe`)    => (p, Miniboxed)
          case (p, `IntTpe`)     => (p, Miniboxed)
          case (p, `LongTpe`)    => (p, Miniboxed)
          case (p, `FloatTpe`)   => (p, Miniboxed)
          case (p, `DoubleTpe`)  => (p, Miniboxed)
          case (p, _)            => (p, Boxed)
        }.toMap
    }
  }
}
