//
//     _____   .__         .__ ___.                    .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_ |__    ____  ___  ___|__|  ____     ____
//   /  \ /  \ |  | /    \ |  | | __ \  /  _ \ \  \/  /|  | /    \   / ___\
//  /    Y    \|  ||   |  \|  | | \_\ \(  <_> ) >    < |  ||   |  \ / /_/  >
//  \____|__  /|__||___|  /|__| |___  / \____/ /__/\_ \|__||___|  / \___  /
//          \/          \/          \/               \/         \/ /_____/
// Copyright (c) 2011-2015 Scala Team, École polytechnique fédérale de Lausanne
//
// Authors:
//    * Milos Stojanovic
//    * Vlad Ureche
//

package miniboxing.plugin
package metadata

trait MiniboxMetadataWarnings {
  self: MiniboxInjectComponent =>

  import global._
  import definitions._

  case class ForwardWarning(pos: Position, mboxedTypeParam: Symbol, nonMboxedType: Type) {
		def warnAndGetSpecInfo(mboxedTpars: Map[Symbol, SpecInfo]): (Symbol, SpecInfo) = {
			(mboxedTypeParam, nonMboxedType) match {
				case (p, tpe) if ScalaValueClasses.contains(tpe.typeSymbol) =>
					(p, Miniboxed(PartialSpec.valueClassRepresentation(tpe.typeSymbol)))

        case (p, TypeRef(_, tpar, _)) if tpar.deSkolemize.isTypeParameter =>
          mboxedTpars.get(tpar.deSkolemize) match {
            case Some(spec: SpecInfo) =>
              (p, spec)

            case None =>
              if (metadata.miniboxedTParamFlag(tpar.deSkolemize) && metadata.isClassStem(tpar.deSkolemize.owner) && !p.isMbArrayMethod)
								(new ForwardWarningForStemClass(p, tpar, pos, inLibrary = !common.isCompiledInCurrentBatch(p))).warn()
              else
								(new ForwardWarningForInnerClass(p, tpar, pos, inLibrary = !common.isCompiledInCurrentBatch(p))).warn()
              (p, Boxed)
          }

        case (p, tpe) if tpe <:< AnyRefTpe =>
          (p, Boxed)

        case (p, tpe) =>
          (new ForwardWarningForNotSpecificEnoughTypeParam(p, tpe, pos)).warn()
        (p, Boxed)
			}
		}
  }

  case class BackwardWarning(pos: Position, nonMboxedTypeParam: Symbol, mboxedType: Type) {
		def warnAndGetSpecInfo(mboxedTpars: Map[Symbol, SpecInfo]): (Symbol, SpecInfo) = {
			(nonMboxedTypeParam, mboxedType) match {
        case (p, tpe) if ScalaValueClasses.contains(tpe.typeSymbol) =>
          (new BackwardWarningForPrimitiveType(p, tpe, pos, inLibrary = !common.isCompiledInCurrentBatch(p))).warn()
          (p, Miniboxed(PartialSpec.valueClassRepresentation(tpe.typeSymbol)))

        case (p, TypeRef(_, tpar, _)) if tpar.deSkolemize.isTypeParameter =>
          mboxedTpars.get(tpar.deSkolemize) match {
            case Some(spec: SpecInfo) =>
              (new BackwardWarningForMiniboxedTypeParam(p, tpar, spec, pos, inLibrary = !common.isCompiledInCurrentBatch(p))).warn()
              (p, spec)

            case None =>
              (p, Boxed)
          }

        case (p, tpe) =>
					(p, Boxed)
      }
		}
  }

  abstract class MiniboxWarning(p: Symbol, pos: Position, inLibrary: Boolean) {

    def msg(): String
    def shouldWarn(): Boolean

    def warn(): Unit = if (shouldWarn) suboptimalCodeWarning(pos, msg, p.isGenericAnnotated, inLibrary)

    def isUselessWarning(p: Symbol): Boolean = {
      p.isMbArrayMethod ||
      p.isImplicitlyPredefMethod ||
      p.isCastSymbol ||
      p.isIsInstanceOfAnyMethod ||
      p.isArrowAssocMethod ||
      p.owner == Tuple1Class ||
      p.owner == Tuple2Class ||
      p.owner == FunctionClass(0) ||
      p.owner == FunctionClass(1) ||
      p.owner == FunctionClass(2)
    }

    def isOwnerArray(p: Symbol, tpe: Type, pos: Position): Boolean = {
      if (p.owner.isArray) {
        (new UseMbArrayInsteadOfArrayWarning(p, tpe: Type, pos)).warn()
        true
      } else false
    }

    def isSpecialized(p: Symbol, pos: Position, inLibrary: Boolean): Boolean = {
      if (p.hasAnnotation(SpecializedClass)) {
        (new ReplaceSpecializedWithMiniboxedWarning(p, pos, inLibrary)).warn()
        true
      } else false
    }
  }

  class BackwardWarningForPrimitiveType(p: Symbol, tpe: Type, pos: Position, inLibrary: Boolean) extends MiniboxWarning(p, pos, inLibrary) {

    override def msg: String = s"The ${p.owner.tweakedFullString} would benefit from miniboxing type " +
                               s"parameter ${p.nameString}, since it is instantiated by a primitive type."

    override def shouldWarn(): Boolean = {
      !isUselessWarning(p.owner) &&
      !isOwnerArray(p, tpe, pos) &&
      !isSpecialized(p, pos, inLibrary)
    }
  }

  class BackwardWarningForMiniboxedTypeParam(p: Symbol, tpar: Symbol, spec: SpecInfo, pos: Position, inLibrary: Boolean) extends MiniboxWarning(p, pos, inLibrary) {

    override def msg: String = s"The ${p.owner.tweakedFullString} would benefit from miniboxing type " +
				                       s"parameter ${p.nameString}, since it is instantiated by miniboxed " +
				                       s"type parameter ${tpar.nameString.stripSuffix("sp")} of " +
				                       s"${metadata.getStem(tpar.owner).tweakedToString}."

    override def shouldWarn(): Boolean = {
      spec != Boxed &&
      !isUselessWarning(p.owner) &&
      !isOwnerArray(p, tpar.tpe, pos) &&
      !isSpecialized(p, pos, inLibrary)
    }
  }

  class ForwardWarningForStemClass(p: Symbol, tpar: Symbol, pos: Position, inLibrary: Boolean) extends MiniboxWarning(p, pos, inLibrary) {

    override def msg: String = "The following code could benefit from miniboxing specialization (the reason was explained before)."

    override def shouldWarn(): Boolean = {
			!isUselessWarning(p.owner)
    }
  }

  class ForwardWarningForInnerClass(p: Symbol, tpar: Symbol, pos: Position, inLibrary: Boolean) extends MiniboxWarning(p, pos, inLibrary) {

    override def msg: String = s"The following code could benefit from miniboxing specialization " +
                               s"if the type parameter ${tpar.name} of ${tpar.owner.tweakedToString} " +
                               s"""would be marked as "@miniboxed ${tpar.name}" (it would be used to """ +
                               s"instantiate miniboxed type parameter ${p.name} of ${p.owner.tweakedToString})"

    override def shouldWarn(): Boolean = {
      !isUselessWarning(p.owner) &&
      !isSpecialized(p, pos, inLibrary)
    }
  }

  class ForwardWarningForNotSpecificEnoughTypeParam(p: Symbol, tpe: Type, pos: Position, inLibrary: Boolean = false) extends MiniboxWarning(p, pos, inLibrary) {

    override def msg: String = s"""Using the type argument "$tpe" for the miniboxed type parameter """ +
                               s"${p.name} of ${p.owner.tweakedToString} is not specific enough, " +
                               s"as it could mean either a primitive or a reference type. Although " +
                               s"${p.owner.tweakedToString} is miniboxed, it won't benefit from " +
                               s"specialization:"

    override def shouldWarn(): Boolean = {
      !isUselessWarning(p.owner) &&
      !isSpecialized(p, pos, inLibrary)
    }
  }

  class UseMbArrayInsteadOfArrayWarning(p: Symbol, tpe: Type, pos: Position, inLibrary: Boolean = false) extends MiniboxWarning(p, pos, inLibrary) {

    override def msg: String = "Use MbArray instead of Array and benefit from miniboxing specialization. " +
															 "For more details about MbArrays, please check the following link: " +
															 "http://scala-miniboxing.org/arrays.html"

    override def shouldWarn(): Boolean = {
      flags.flag_warn_mbarrays &&
      p.owner.isArray &&
      (ScalaValueClasses.contains(tpe.typeSymbol) || tpe.typeSymbol.deSkolemize.hasAnnotation(MinispecClass))
    }
  }

  class ReplaceSpecializedWithMiniboxedWarning(p: Symbol, pos: Position, inLibrary: Boolean) extends MiniboxWarning(p, pos, inLibrary) {

    override def msg: String = s"Although the type parameter ${p.nameString} of ${p.owner.tweakedFullString} is " +
                                "specialized, miniboxing and specialization communicate among themselves by boxing " +
                                "(thus, inefficiently) on all classes other than as FunctionX and TupleX. If you " +
                                "want to maximize performance, consider switching from specialization to miniboxing: " +
                                "'@miniboxed T':"

    override def shouldWarn(): Boolean = {
      p.hasAnnotation(SpecializedClass)
    }
  }
}
