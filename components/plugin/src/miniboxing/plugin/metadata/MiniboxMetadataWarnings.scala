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
  import reflect.internal.Flags._

  def isArrayTypeParam(typeParam: Symbol): Boolean =
    typeParam.isTypeParameterOrSkolem &&
    typeParam.owner.isArray

  def isSpecializedTypeParam(specializedParam: Symbol): Boolean =
    // only warn for @miniboxed instantiations, not for primitives:
    specializedParam.isTypeParameterOrSkolem &&
    specializedParam.deSkolemize.hasFlag(SPECIALIZED)

  /** A miniboxed type parameter is instantiated by a non-miniboxed (and non-primitive) type */
  def forwardWarning(mboxedTypeParam: Symbol,
                     nonMboxedType: Type,
                     pos: Position,
                     warningType: ForwardWarningEnum.Value,
                     inLibrary: Boolean): Option[MiniboxWarning] =
    if (metadata.getStemTypeParam(mboxedTypeParam) != mboxedTypeParam)
      forwardWarning(metadata.getStemTypeParam(mboxedTypeParam), nonMboxedType, pos, warningType, inLibrary)
    else
      if (metadata.miniboxedTParamFlag(mboxedTypeParam))
        if (isSpecializedTypeParam(nonMboxedType.typeSymbol))
          Some(ReplaceSpecializedWithMiniboxedWarning(nonMboxedType.typeSymbol, mboxedTypeParam, pos, inLibrary))
        else
          warningType match {
            case ForwardWarningEnum.VariantClass =>
              Some(ForwardWarningForVariantClass(mboxedTypeParam, nonMboxedType, pos, inLibrary))
            case ForwardWarningEnum.StemClass =>
              Some(ForwardWarningForStemClass(mboxedTypeParam, nonMboxedType, pos, inLibrary))
            case ForwardWarningEnum.NotSpecificEnoughTypeParam =>
              Some(ForwardWarningForNotSpecificEnoughTypeParam(mboxedTypeParam, nonMboxedType, pos, inLibrary))
            case _ =>
              None
          }
      else
        None

  object ForwardWarningEnum extends Enumeration {
    val VariantClass, StemClass, NotSpecificEnoughTypeParam = Value
  }

  def backwardWarning(nonMboxedTypeParam: Symbol,
                      mboxedTypeParamOrPrimitiveSym: Symbol,
                      pos: Position,
                      warningType: BackwardWarningEnum.Value,
                      inLibrary: Boolean): Option[MiniboxWarning] =

    if (metadata.getStemTypeParam(mboxedTypeParamOrPrimitiveSym) != mboxedTypeParamOrPrimitiveSym)
      backwardWarning(nonMboxedTypeParam, metadata.getStemTypeParam(mboxedTypeParamOrPrimitiveSym), pos, warningType, inLibrary)
    else
      if (!metadata.miniboxedTParamFlag(nonMboxedTypeParam)) {
        if (isArrayTypeParam(nonMboxedTypeParam))
          Some(ReplaceArrayByMbArrayBackwardWarning(nonMboxedTypeParam, mboxedTypeParamOrPrimitiveSym, pos))
        else if (isSpecializedTypeParam(nonMboxedTypeParam))
          Some(ReplaceSpecializedWithMiniboxedWarning(nonMboxedTypeParam, mboxedTypeParamOrPrimitiveSym, pos, inLibrary))
        else warningType match {
          case BackwardWarningEnum.PrimitiveType =>
            Some(BackwardWarningForPrimitiveType(nonMboxedTypeParam, mboxedTypeParamOrPrimitiveSym, pos, inLibrary))
          case BackwardWarningEnum.MiniboxedTypeParam =>
            Some(BackwardWarningForMiniboxedTypeParam(nonMboxedTypeParam, mboxedTypeParamOrPrimitiveSym, pos, inLibrary))
          case _ =>
            None
        }
      } else None



  object BackwardWarningEnum extends Enumeration {
    val PrimitiveType, MiniboxedTypeParam = Value
  }

  abstract class MiniboxWarning(typeParam: Option[Symbol], pos: Position, inLibrary: Boolean) {

    if (typeParam.isDefined)
      assert(metadata.getStemTypeParam(typeParam.get) == typeParam.get, typeParam + "  " + getClass)

    def msg(): String

    def shouldWarnPredef(): Boolean =
      flags.flag_strict_warnings &&
      (pos != NoPosition) &&
      !(typeParam.isDefined && typeParam.get.isGenericAnnotated) &&
      (!inLibrary || flags.flag_strict_warnings_outside)

    def shouldWarn(): Boolean

    def warn(): Unit =
      if (shouldWarnPredef && shouldWarn && !alreadyWarnedTypeParam && !alreadyWarnedPosition) {
        if (typeParam.isDefined)
          metadata.warningTypeParameters += typeParam.get
        metadata.warningPositions += pos
        suboptimalCodeWarning(pos, msg)
      }

    lazy val alreadyWarnedTypeParam: Boolean =
      if (typeParam.isEmpty) false
      else metadata.warningTypeParameters.contains(typeParam.get)

    lazy val alreadyWarnedPosition: Boolean =
      metadata.warningPositions.contains(pos)

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
  }

  case class BackwardWarningForPrimitiveType(nonMboxedTypeParam: Symbol, primitiveType: Symbol, pos: Position, inLibrary: Boolean)
    extends MiniboxWarning(Some(nonMboxedTypeParam), pos, inLibrary) {

    override def msg: String = s"The ${nonMboxedTypeParam.owner.tweakedFullString} would benefit from miniboxing type " +
                               s"parameter ${nonMboxedTypeParam.nameString}, since it is instantiated by a primitive type."

    def shouldWarn(): Boolean = {
      !isUselessWarning(nonMboxedTypeParam.owner)
    }
  }

  case class BackwardWarningForMiniboxedTypeParam(nonMboxedTypeParam: Symbol, mboxedTypeParam: Symbol, pos: Position, inLibrary: Boolean)
    extends MiniboxWarning(Some(nonMboxedTypeParam), pos, inLibrary) {

    override def msg: String = s"The ${nonMboxedTypeParam.owner.tweakedFullString} would benefit from miniboxing type " +
                               s"parameter ${nonMboxedTypeParam.nameString}, since it is instantiated by miniboxed " +
                               s"type parameter ${mboxedTypeParam.nameString.stripSuffix("sp")} of " +
                               s"${metadata.getStem(mboxedTypeParam.owner).tweakedToString}."

    def shouldWarn(): Boolean =
      !isUselessWarning(nonMboxedTypeParam.owner)
  }

  case class ForwardWarningForStemClass(mboxedTypeParam: Symbol, nonMboxedType: Type, pos: Position, inLibrary: Boolean)
    extends MiniboxWarning(Some(mboxedTypeParam), pos, inLibrary) {

    override def msg: String = "The following code could benefit from miniboxing specialization (the reason was explained before)."

    def shouldWarn(): Boolean = {
      !isUselessWarning(mboxedTypeParam.owner)
    }
  }

  case class ForwardWarningForVariantClass(mboxedTypeParam: Symbol, nonMboxedType: Type, pos: Position, inLibrary: Boolean)
    extends MiniboxWarning(Some(mboxedTypeParam), pos, inLibrary) {

    override def msg: String = s"The following code could benefit from miniboxing specialization " +
                               s"if the type parameter ${nonMboxedType.typeSymbol.name} of ${nonMboxedType.typeSymbol.owner.tweakedToString} " +
                               s"""would be marked as "@miniboxed ${nonMboxedType.typeSymbol.name}" (it would be used to """ +
                               s"instantiate miniboxed type parameter ${mboxedTypeParam.name} of ${mboxedTypeParam.owner.tweakedToString})"

    def shouldWarn(): Boolean =
      !isUselessWarning(mboxedTypeParam.owner)
  }

  case class ForwardWarningForNotSpecificEnoughTypeParam(mboxedTypeParam: Symbol, nonMboxedType: Type, pos: Position, inLibrary: Boolean = false)
    extends MiniboxWarning(Some(mboxedTypeParam), pos, inLibrary) {

    override def msg: String = s"""Using the type argument "$nonMboxedType" for the miniboxed type parameter """ +
                               s"${mboxedTypeParam.name} of ${mboxedTypeParam.owner.tweakedToString} is not specific enough, " +
                               s"as it could mean either a primitive or a reference type. Although " +
                               s"${mboxedTypeParam.owner.tweakedToString} is miniboxed, it won't benefit from " +
                               s"specialization:"

    def shouldWarn(): Boolean =
      !isUselessWarning(mboxedTypeParam.owner)
  }

  object ReplaceArrayByMbArrayBackwardWarning {
    val warnedFiles = perRunCaches.newSet[scala.reflect.io.AbstractFile]
  }

  case class ReplaceArrayByMbArrayBackwardWarning(typeParam: Symbol, miniboxedTypeArgument: Symbol, pos: Position, inLibrary: Boolean = false)
    extends MiniboxWarning(Some(metadata.getStemTypeParam(miniboxedTypeArgument)), pos, inLibrary) {

    import ReplaceArrayByMbArrayBackwardWarning._

    override def msg: String = "Use MbArray instead of Array to eliminate the need for ClassTags and " +
                               "benefit from seamless interoperability with the miniboxing specialization. " +
                               "For more details about MbArrays, please check the following link: " +
                               "http://scala-miniboxing.org/arrays.html"

    override def warn() =
      // make sure we don't overload the user => warn once per file
      if (!warnedFiles.contains(miniboxedTypeArgument.associatedFile)) {
        super.warn()
        warnedFiles += miniboxedTypeArgument.associatedFile
      }

    // alternative: use the counter
    override lazy val alreadyWarnedTypeParam = false

    def shouldWarn(): Boolean = {
      flags.flag_warn_mbarrays &&
      ((typeParam.owner.isArray || (typeParam.owner == ArrayModule_genericApply)) &&
      miniboxedTypeArgument.deSkolemize.hasAnnotation(MinispecClass) || typeParam.owner.isClassTag)
    }
  }

  case class ReplaceSpecializedWithMiniboxedWarning(specializedTParam: Symbol, tArg: Symbol, pos: Position, inLibrary: Boolean)
    extends MiniboxWarning(Some(specializedTParam), pos, inLibrary) {

    override def msg: String = s"Although the type parameter ${specializedTParam.nameString} of ${specializedTParam.owner.tweakedFullString} is " +
                                "specialized, miniboxing and specialization communicate among themselves by boxing " +
                                "(thus, inefficiently) on all classes other than as FunctionX and TupleX. If you " +
                                "want to maximize performance, consider switching from specialization to miniboxing: " +
                                "'@miniboxed T':"

    def shouldWarn(): Boolean =
      specializedTParam.hasAnnotation(SpecializedClass) &&
      tArg.isTypeParameterOrSkolem &&
      metadata.getStemTypeParam(tArg.deSkolemize).isMiniboxAnnotated

  }

  case class AmbiguousMbArrayTypeArgumentWarning(pos: Position)
      extends MiniboxWarning(None, pos, false) {

    override def msg: String = "The following code instantiating an `MbArray` object cannot be optimized since the " +
                               "type argument is not a primitive type (like Int), a miniboxed type parameter or a " +
                               "subtype of AnyRef. This means that primitive types could end up boxed:"

    def shouldWarn(): Boolean = true
  }

  case class ReplaceTypeClassByMbTypeClassWarning(pos: Position,
                                                  typeParam: Option[Symbol],
                                                  typeClassOriginal: Symbol,
                                                  typeClassReplacement: Symbol,
                                                  miniboxedTypeParameterOrPrimitive: Type)
      extends MiniboxWarning(typeParam, pos, false) {

    private[this] val targ = if (typeParam.isDefined) typeParam.get.tpeHK else miniboxedTypeParameterOrPrimitive
    override def msg: String = "Upgrade from " + typeClassOriginal + "[" + targ + "]" + " to " +
                               typeClassReplacement + "[" + targ + "] to benefit from miniboxing specialization."

    def shouldWarn(): Boolean = true
  }

  case class InnerClassNotSpecializedWarning(pos: Position,
                                             outerClass: Symbol,
                                             innerClass: Symbol)
      extends MiniboxWarning(None, pos, false) {

    override def msg: String = "The " + innerClass.tweakedToString + " will not be miniboxed based " +
                               "on type parameter(s) " + outerClass.typeParams.map(_.nameString).mkString(", ") +
                               " of miniboxed " + outerClass.tweakedToString + ". To have it specialized, " +
                               "add the type parameters of " + outerClass.tweakedToString + ", marked with " +
                               "\"@miniboxed\" to the definition of " + innerClass.tweakedToString +
                               " and instantiate it explicitly passing the type parameters from " +
                               outerClass.tweakedToString + ":"

    def shouldWarn(): Boolean = !innerClass.isGenericAnnotated
  }

  case class MissingAliasForMemberWarning(pos: Position,
                                          alias: Symbol,
                                          stemClass: Symbol)
      extends MiniboxWarning(None, pos, !common.isCompiledInCurrentBatch(stemClass)) {

    override def msg: String = "The " + alias + " in " + alias.owner + " is called from " + stemClass + " " +
                               "using the `super." + alias.nameString + "` construction. However, after " +
                               "miniboxing, this construction becomes suboptimal, since there is no specialized " +
                               "variant of " + alias + " exactly matching the specialization in " + stemClass +
                               ". To fix this, make sure that the specializations of " + stemClass + " and " +
                               alias.owner + " match exactly.\nFor more information, please see " +
                               "https://github.com/miniboxing/miniboxing-plugin/issues/73:"

    def shouldWarn(): Boolean = true
  }
}
