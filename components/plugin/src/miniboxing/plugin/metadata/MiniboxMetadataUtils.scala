package miniboxing.plugin
package metadata

import scala.Option.option2Iterable
import scala.collection.immutable

trait MiniboxMetadataUtils {
  self: MiniboxDuplComponent =>

  import global._
  import definitions._
  import scala.collection.immutable

  def createNewTParams(oldParams: List[Symbol], newOwner: Symbol): Map[Symbol, Symbol] = {
    def newName(p: Symbol): Name = p.name.append("sp")
    val newParams = oldParams map (p => p.cloneSymbol(newOwner, p.flags, newName(p)))

    // Update references to old type parameters to the new type parameters
    // See https://github.com/miniboxing/miniboxing-plugin/issues/36 for details.
    newParams.map(_.modifyInfo(info => info.substituteSymbols(oldParams, newParams)))

    newParams foreach (p => { p.removeAnnotation(MinispecClass); p.removeAnnotation(SpecializedClass) })
    (oldParams zip newParams).toMap
  }

  implicit class RichPartialSpec(pspec: PartialSpec) {
    def toAllBoxed: PartialSpec = pspec.keys.map(tp => (tp, Boxed)).toMap
  }

  object variants {
    /**
     * For a set of type parameters, get all the possible partial specializations.
     *
     * A partial specialization is represented as a list that gives the types that
     * specifies what a type parameter is instantiated to.
     */
    def allSpecializations(tParams: List[Symbol]): List[PartialSpec] = {
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
  }

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

  object typeMappers {

    class SubstSkolemsTypeMap(from: List[Symbol], to: List[Type]) extends SubstTypeMap(from, to) {
      protected override def matches(sym1: Symbol, sym2: Symbol) =
        if (sym2.isTypeSkolem) sym2.deSkolemize eq sym1
        else sym1 eq sym2
    }

    case class MiniboxSubst(env: Map[Symbol, Type]) extends TypeMap {
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
    private def substParams(pmap: Map[Symbol, Symbol])(tpe: Type): Type = {
      val (oldTParams, newTParams) = pmap.toList.unzip
      tpe.instantiateTypeParams(oldTParams, newTParams map (_.tpe))
    }
  }

  object parentClasses {
    class SpecializeTypeMap(current: Symbol) extends TypeMap {
      import metadata._

      // TODO: Take owneship chain into account!
      def extractPSpec(tref: TypeRef) = PartialSpec.fromType(tref)

      override def apply(tp: Type): Type = tp match {
        case tref@TypeRef(pre, sym, args) if args.nonEmpty =>
          val pre1 = this(pre)
          afterMiniboxDupl(sym.info)
          classOverloads(sym).get(extractPSpec(tref)) match {
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
  }

  object tags {
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
  }

  object heuristics {
    def hasSpecializedArgumentsOrReturn(clazz: Symbol, member: Symbol) =
      flag_spec_no_opt || {
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

    // shamelessly stolen from specialization
    def specializableClass(tp: Type): Boolean = (
         !definitions.isRepeatedParamType(tp)
      && !tp.typeSymbol.isJavaDefined
      && !tp.typeSymbol.isPackageClass
    )

    def specializableMethodInClass(clazz: Symbol, mbr: Symbol): Boolean = (
         (!mbr.isMethod
      || !mbr.isSynthetic)
      && ((mbr.alias == NoSymbol)
      || metadata.memberOverloads.isDefinedAt(mbr.alias))
    )

    def normalizableMethodInMethod(sym: Symbol): Boolean = (
         sym.isMethod
      && sym.hasMiniboxedTypeParameters
      && sym.owner.isMethod
    )

  }
}
