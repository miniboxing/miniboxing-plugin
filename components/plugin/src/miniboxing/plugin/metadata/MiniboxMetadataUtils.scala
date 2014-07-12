//
//     _____   .__         .__ ___.                    .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_ |__    ____  ___  ___|__|  ____     ____
//   /  \ /  \ |  | /    \ |  | | __ \  /  _ \ \  \/  /|  | /    \   / ___\
//  /    Y    \|  ||   |  \|  | | \_\ \(  <_> ) >    < |  ||   |  \ / /_/  >
//  \____|__  /|__||___|  /|__| |___  / \____/ /__/\_ \|__||___|  / \___  /
//          \/          \/          \/               \/         \/ /_____/
// Copyright (c) 2012-2014 Scala Team, École polytechnique fédérale de Lausanne
//
// Authors:
//    * Iulian Dragos
//    * Cristian Talau
//    * Vlad Ureche
//
package miniboxing.plugin
package metadata

import scala.Option.option2Iterable
import scala.collection.immutable

trait MiniboxMetadataUtils {
  self: MiniboxInjectComponent =>

  import global._
  import definitions._
  import scala.collection.immutable
  import scala.collection.mutable.HashMap

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
          envs = envs.flatMap(rest =>
            if (!flag_two_way)
              List(Miniboxed(LongClass) :: rest, Boxed :: rest)
            else
              List(Miniboxed(LongClass) :: rest, Miniboxed(DoubleClass) :: rest, Boxed :: rest))
      envs.map((types: List[SpecInfo]) => (mboxTParams zip types).toMap)
    }

    def typeParamValues(clazz: Symbol, env: PartialSpec): List[Type] =
      clazz.typeParams.filter(metadata.miniboxedTParamFlag(_)).map(env) map {
        case Boxed           => AnyRefClass.tpe
        case Miniboxed(repr) => repr.tpe
      }
  }

  object PartialSpec {

    def isAllAnyRef(env: PartialSpec) = !env.isEmpty && env.forall(_._2 == Boxed)

    def allAnyRefPSpec(clazz: Symbol): PartialSpec = clazz.typeParams.filter(_.isMiniboxAnnotated).map(t => (t, Boxed)).toMap

    def fromType(tpe: TypeRef, owner: Symbol): PartialSpec = fromType(tpe, owner, Map.empty)
    def fromType(tpe: TypeRef, owner: Symbol, pspec: PartialSpec): PartialSpec =
      tpe match {
        case TypeRef(pre, sym, targs) =>
          val tparams = sym.info.typeParams
          fromTargs(tparams, targs, owner, pspec)
      }

    def fromTargs(tparams: List[Symbol], targs: List[Type], currentOwner: Symbol, pspec: PartialSpec = Map.empty): PartialSpec =
      fromTargsAllTargs(tparams, targs, currentOwner, pspec).collect({ case (tparam, spec) if metadata.miniboxedTParamFlag(tparam) => (tparam, spec) })

    def fromTargsAllTargs(tparams: List[Symbol], targs: List[Type], currentOwner: Symbol, pspec: PartialSpec = Map.empty): PartialSpec = {

      def typeParametersFromOwnerChain(owner: Symbol = currentOwner): List[(Symbol, SpecInfo)] =
        owner.ownerChain flatMap { sym =>
          // for methods => normalization
          // for classes => specialization
          afterMiniboxInject(owner.info) // make sure it's specialized
          if (sym.isMethod && !sym.typeParams.isEmpty) {
            // NOTE: We could also rely on the method's specialization, but we rely on the
            // assumption that a class' specialization is the one that dominates, else we
            // would be messing up forwarders.
            val localPSpec = metadata.getNormalLocalSpecialization(sym)
            localPSpec.collect({ case (tp, spec) if spec != Boxed => (tp, spec)})
          } else if (sym.isClass || sym.isTrait) {
            val localPSpec = metadata.getClassLocalSpecialization(sym)
            localPSpec.collect({ case (tp, spec) if spec != Boxed => (tp, spec)})
          } else
            Nil
        }

      val mboxedTpars = typeParametersFromOwnerChain().toMap ++ pspec
      val spec = (tparams zip targs) flatMap { (pair: (Symbol, Type)) =>
        val FloatRepr = if (flag_two_way) DoubleClass else LongClass
        pair match {
          // case (2.3)
          case (p, `CharTpe`)    => Some((p, Miniboxed(LongClass)))
          case (p, `IntTpe`)     => Some((p, Miniboxed(LongClass)))
          case (p, `LongTpe`)    => Some((p, Miniboxed(LongClass)))
          case (p, `FloatTpe`)   => Some((p, Miniboxed(FloatRepr)))
          case (p, `DoubleTpe`)  => Some((p, Miniboxed(FloatRepr)))
          // case (2.1)
          // case (2.2)
          // case (2.4)
          case (p, TypeRef(_, tpar, _)) =>
            mboxedTpars.get(tpar.deSkolemize) match {
              case Some(spec: SpecInfo) => Some((p, spec))
              case None                 => Some((p, Boxed))
            }
          case _                 => None
        }
      }
      spec.toMap
    }
  }

  object typeMappers {

    class SubstSkolemsTypeMap(from: List[Symbol], to: List[Type]) extends SubstTypeMap(from, to) {
      protected override def matches(sym1: Symbol, sym2: Symbol) =
        if (sym2.isTypeSkolem) sym2.deSkolemize eq sym1
        else sym1 eq sym2
      override def toString() =
        s"SubstSkolemsTypeMap(${from zip to})"
    }

    case class MiniboxSubst(env: Map[Symbol, Type]) extends TypeMap {
      val (keys, shallowTypes) = env.toList.unzip
      val deepTypes = shallowTypes.map(_.filterAnnotations(_.tpe.typeSymbol != StorageClass))
      val deepSubst = new SubstSkolemsTypeMap(keys, deepTypes)
      val shallowSubst = new SubstSkolemsTypeMap(keys, shallowTypes) {
        override def mapOver(tp: Type) = {
          val res = tp match {
            case TypeRef(pre, sym, args) if (!keys.exists(sym2 => matches(sym2, sym))) =>
              deepSubst(tp)
            case TypeBounds(lo, hi) =>
              val lo1 = deepSubst(lo)
              val hi1 = deepSubst(hi)
              if ((lo1 eq lo) && (hi1 eq hi)) tp
              else TypeBounds(lo1, hi1)
            case _ =>
              super.mapOver(tp)
          }
          res
        }
      }
      def apply(tp: Type) = shallowSubst(tp)
      def mapOverDeep(tp: Type) = deepSubst(tp)
      override def toString() =
        s"MiniboxSubst with shallow=${shallowSubst} and deep=${deepSubst}"
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
      def extractPSpec(tref: TypeRef) = PartialSpec.fromType(tref, current.owner)

      override def apply(tp: Type): Type = tp match {
        case tref@TypeRef(pre, sym, args) if args.nonEmpty && classOverloads.isDefinedAt(sym)=>
          val pre1 = this(pre)
          afterMiniboxInject(sym.info)
          classOverloads(sym).get(extractPSpec(tref)) match {
            case Some(sym1) =>
              val localTParamMap = (sym1.typeParams zip args.map(_.typeSymbol)).toMap
              inheritedDeferredTypeTags.getOrElseUpdate(current, HashMap()) ++=
                primaryDeferredTypeTags.getOrElse(sym1, HashMap()).mapValues(s => localTParamMap.getOrElse(s, s)) ++
                inheritedDeferredTypeTags.getOrElse(sym1, HashMap()).mapValues(s => localTParamMap.getOrElse(s, s))
              typeRef(pre1, sym1, args)
            case None       => typeRef(pre1, sym, args)
          }
        case _ => tp
      }
    }

    def specializeParentsTypeMapForGeneric(current: Symbol) = new SpecializeTypeMap(current)

    def specializeParentsTypeMapForSpec(spec: Symbol, origin: Symbol, pspec: PartialSpec) = new SpecializeTypeMap(spec) {
      override def extractPSpec(tref: TypeRef) = PartialSpec.fromType(tref, spec.owner, pspec)
      override def apply(tp: Type): Type = tp match {
        case tref@TypeRef(pre, sym, args) if sym == origin =>
          tref
        case _ =>
          super.apply(tp)
      }
    }
  }

  object tagUtils {
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
        val tparams = clazz.typeParams.filter(s => new RichSym(s).isMiniboxAnnotated)
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
      clazz.typeParams.exists(s => new RichSym(s).isMiniboxAnnotated)

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
