package miniboxing.plugin

import scala.tools.nsc.Global
import scala.collection.immutable.ListMap

trait MiniboxLogic {
  self: MiniboxDuplComponent =>

  import global._
  import definitions._
  import scala.collection.immutable

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
  implicit class RichPartialSpec(pspec: PartialSpec) {
    def allAnyRef: PartialSpec = pspec.keys.map(tp => (tp, Boxed)).toMap
  }

  /**
   * For a set of type parameters, get all the possible partial specializations.
   *
   * A partial specialization is represented as a list that gives the types that
   * specifies what a type parameter is instantiated to.
   */
  def specializations(tParams: List[Symbol]): List[PartialSpec] = {
    var envs: List[List[SpecInfo]] = List(Nil)
    val mboxTParams = tParams.filter(s => s.hasAnnotation(MinispecClass) || s.hasAnnotation(SpecializedClass))

    for (tParam <- mboxTParams)
      if (tParam.hasAnnotation(MinispecClass) || tParam.hasAnnotation(SpecializedClass))
        envs = envs.flatMap(rest => List(Miniboxed :: rest, Boxed :: rest))

    envs.map((types: List[SpecInfo]) => (mboxTParams zip types).toMap)
  }

  /**
   * Specialize name for the two list of types.
   */
  def specializedName(name: Name, types: List[Type]): TermName = {
    if (nme.CONSTRUCTOR == name || (types.isEmpty))
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
  def typeTagName(clazz: Symbol, tparam: Symbol): TermName =
    // See #55 for an explanation of why I did this: https://github.com/miniboxing/miniboxing-plugin/issues/55
    newTermName(clazz.fullName('|') + "|" + shortTypeTagName(tparam))
    // nme.expandedName(shortTypeTagName(tparam), clazz, "|")

  def shortTypeTagName(tparam: Symbol): TermName =
    newTermName(tparam.name.toString + "_TypeTag")

  def isTypeTagField(field: Symbol): Boolean = {
    field.name.endsWith("_TypeTag")
  }

  def typeParamValues(clazz: Symbol, env: PartialSpec): List[Type] =
    clazz.typeParams.filter(_.hasFlag(MINIBOXED)).map(env) map {
      case Boxed => AnyRefClass.tpe
      case Miniboxed => LongClass.tpe
    }

  def needsSpecialization(clazz: Symbol, member: Symbol) = flag_spec_no_opt || {
    val tparams = clazz.typeParams.filter(isSpecialized(clazz, _))
    val res = member.info.paramss.flatten.exists(mbr => tparams.contains(mbr.info.typeSymbol.deSkolemize)) ||
    tparams.contains(member.info.finalResultType.typeSymbol.deSkolemize)
    res
  }


  /**
   * Tells whether a class must be specialized by looking at the annotations
   */
  def isSpecializableClass(clazz: Symbol) =
    clazz.isClass &&
    !clazz.typeParams.isEmpty &&
    clazz.typeParams.exists(isSpecialized(clazz, _))

  def isSpecialized(clazz: Symbol, tparam: Symbol): Boolean = {
    beforeMinibox(tparam.info) // make sure the annotation hijacker updated it
    tparam hasAnnotation MinispecClass
  }

  final val MINIBOXED = 1L << 46 // we define our own flag

  object PartialSpec {

    def isAllAnyRef(env: PartialSpec) = !env.isEmpty && env.forall(_._2 == Boxed)

    def allAnyRefPSpec(clazz: Symbol): PartialSpec = clazz.typeParams.filter(isSpecialized(clazz, _)).map(t => (t, Boxed)).toMap

    // used if the current class is not miniboxed
    def fromType(tpe: TypeRef): PartialSpec = fromTypeInContext(tpe, Map.empty)

    // used if the current class is miniboxed
    def fromTypeInContext(tpe: TypeRef, pspec: PartialSpec): PartialSpec = tpe match {
      case TypeRef(pre, sym, args) =>
        val tparams = sym.info.typeParams
        ((tparams zip args) flatMap { (pair: (Symbol, Type)) =>
          pair match {
            case (p, _) if !(p hasFlag MINIBOXED) => None
            case (p, `UnitTpe`)    => Some((p, Miniboxed))
            case (p, `BooleanTpe`) => Some((p, Miniboxed))
            case (p, `ByteTpe`)    => Some((p, Miniboxed))
            case (p, `ShortTpe`)   => Some((p, Miniboxed))
            case (p, `CharTpe`)    => Some((p, Miniboxed))
            case (p, `IntTpe`)     => Some((p, Miniboxed))
            case (p, `LongTpe`)    => Some((p, Miniboxed))
            case (p, `FloatTpe`)   => Some((p, Miniboxed))
            case (p, `DoubleTpe`)  => Some((p, Miniboxed))
            case (p, tpe)          => Some((p, pspec.getOrElse(tpe.typeSymbol, Boxed)))
          }
        }).toMap
    }
  }

  def notSpecializable(clazz: Symbol, mbr: Symbol) =
    mbr.isMethod && mbr.isSynthetic ||
    (mbr.alias != NoSymbol) && !(overloads.isDefinedAt(mbr.alias))
}
