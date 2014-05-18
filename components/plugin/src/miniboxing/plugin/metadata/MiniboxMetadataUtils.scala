package miniboxing.plugin
package metadata

import scala.Option.option2Iterable
import scala.collection.immutable

trait MiniboxMetadataUtils {
  self: MiniboxDuplComponent =>

  import global._
  import definitions._
  import scala.collection.immutable

  object ParamMap {
    def apply(oldParams: List[Symbol], newOwner: Symbol): ParamMap = {
      def newName(p: Symbol): Name = p.name.append("sp")
      val newParams = oldParams map (p => p.cloneSymbol(newOwner, p.flags, newName(p)))

      // Update references to old type parameters to the new type parameters
      // See https://github.com/miniboxing/miniboxing-plugin/issues/36 for details.
      newParams.map(_.modifyInfo(info => info.substituteSymbols(oldParams, newParams)))

      newParams foreach (p => { p.removeAnnotation(MinispecClass); p.removeAnnotation(SpecializedClass) })
      (oldParams zip newParams).toMap
    }
  }

  implicit class RichPartialSpec(pspec: PartialSpec) {
    def isAllBoxed: PartialSpec = pspec.keys.map(tp => (tp, Boxed)).toMap
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

  def typeParamValues(clazz: Symbol, env: PartialSpec): List[Type] =
    clazz.typeParams.filter(_.hasFlag(MINIBOXED)).map(env) map {
      case Boxed => AnyRefClass.tpe
      case Miniboxed => LongClass.tpe
    }

  def needsSpecialization(clazz: Symbol, member: Symbol) = flag_spec_no_opt || {
    val tparams = clazz.typeParams.filter(_.isMiniboxAnnotated)
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
    clazz.typeParams.exists(_.isMiniboxAnnotated)

  // Thanks to @xeno-by :)
  implicit class RichSym(sym: Symbol) {
    def getMiniboxedTypeParameters: List[Symbol] =
      sym.typeParams.filter((s: Symbol) => s.isMiniboxAnnotated)
    def hasMiniboxedTypeParameters: Boolean =
      sym.typeParams.exists((s: Symbol) => s.isMiniboxAnnotated)
    def isMiniboxAnnotated: Boolean = {
      beforeMiniboxDupl(sym.info) // make sure the annotation hijacker updated it
      sym hasAnnotation MinispecClass
    }
  }

  object PartialSpec {

    def isAllAnyRef(env: PartialSpec) = !env.isEmpty && env.forall(_._2 == Boxed)

    def allAnyRefPSpec(clazz: Symbol): PartialSpec = clazz.typeParams.filter(_.isMiniboxAnnotated).map(t => (t, Boxed)).toMap

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
    (mbr.alias != NoSymbol) && !(specializedMembers.isDefinedAt(mbr.alias))
}
