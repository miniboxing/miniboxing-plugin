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
//    * Vlad Ureche
//    * Cristian Talau
//
package miniboxing.plugin
package metadata

import scala.tools.nsc.plugins.PluginComponent
import scala.collection.immutable.ListMap
import miniboxing.runtime.MiniboxConstants._

trait MiniboxDefinitions {
  this: PluginComponent =>

  import global._
  import definitions._
  import miniboxing.runtime.MiniboxConstants._

  // Specialization/normalization info:

  /**  PartialSpec is a binding from type parameters to their representation (Boxed/Miniboxed)
   *   INVARIANT: Regardless of whether the PartialSpec refers to the stem or a variant class,
   *   the parent's type parameters are used. */
  type PartialSpec = Map[Symbol, SpecInfo]

  sealed trait SpecInfo
  case object Miniboxed extends SpecInfo
  case object Boxed     extends SpecInfo


  // Flags and symbols:

  final val MINIBOXED = 1L << 46 // we define our own flag

  lazy val MinispecClass = rootMirror.getRequiredClass("scala.miniboxed")
  /**
   * This class should only appear in the tree during the `minibox` phase
   * and should be cleaned up afterwards, during the `minibox-cleanup` phase.
   */
  lazy val StorageClass = {
    // This is what is should look like:
    // ```
    //   package __root__.scala {
    //     class storage[Tpe] extends Annotation with TypeConstraint
    //   }
    // ```
    val AnnotationName = "scala.annotation.Annotation"
    val TypeConstrName = "scala.annotation.TypeConstraint"
    val AnnotationTpe = rootMirror.getRequiredClass(AnnotationName).tpe
    val TypeConstrTpe = rootMirror.getRequiredClass(TypeConstrName).tpe

    val StorageName = newTypeName("storage")
    val StorageSym = ScalaPackageClass.newClassSymbol(StorageName, NoPosition, 0L)
    val TypeParamName = newTypeName("Tpe")
    val TypeParamSym = StorageSym.newTypeParameter(TypeParamName, NoPosition, 0L) setInfo TypeBounds.empty
    StorageSym setInfoAndEnter PolyType(List(TypeParamSym), ClassInfoType(List(AnnotationTpe, TypeConstrTpe), newScope, StorageSym))
    StorageSym
  }


  // array ops
  lazy val MiniboxArrayObjectSymbol = rootMirror.getRequiredModule("miniboxing.runtime.MiniboxArray")
  lazy val mbarray_update = definitions.getMember(MiniboxArrayObjectSymbol, newTermName("mbarray_update_minibox"))
  lazy val mbarray_apply  = definitions.getMember(MiniboxArrayObjectSymbol, newTermName("mbarray_apply_minibox"))
  lazy val mbarray_length = definitions.getMember(MiniboxArrayObjectSymbol, newTermName("mbarray_length"))
  lazy val mbarray_new    = definitions.getMember(MiniboxArrayObjectSymbol, newTermName("mbarray_new"))

  // Any ops
  lazy val TagDipatchObjectSymbol = rootMirror.getRequiredModule("miniboxing.runtime.MiniboxDispatch")
  lazy val tag_hashCode = definitions.getMember(TagDipatchObjectSymbol, newTermName("mboxed_hashCode"))
  lazy val other_==     = definitions.getMember(TagDipatchObjectSymbol, newTermName("mboxed_eqeq_other"))
  lazy val tag_==       = definitions.getMember(TagDipatchObjectSymbol, newTermName("mboxed_eqeq_tag"))
  lazy val notag_==     = definitions.getMember(TagDipatchObjectSymbol, newTermName("mboxed_eqeq_notag"))
  lazy val tag_toString = definitions.getMember(TagDipatchObjectSymbol, newTermName("mboxed_toString"))

  // conversions
  lazy val ConversionsObjectSymbol = rootMirror.getRequiredModule("miniboxing.runtime.MiniboxConversions")
  // type tag conversions
  lazy val minibox2box        = definitions.getMember(ConversionsObjectSymbol, newTermName("minibox2box"))
  lazy val box2minibox        = definitions.getMember(ConversionsObjectSymbol, newTermName("box2minibox_tt"))

  // artificially created marker methods
  // def marker_minibox2box[T, St](t: T @storage[St]): T
  // def marker_box2minibox[T, St](t: T): T @storage[St]
  lazy val marker_minibox2box =
    newPolyMethod(2, ConversionsObjectSymbol, newTermName("marker_minibox2box"), 0L)(
      tpar => (Some(List(tpar(0).tpeHK withAnnotation AnnotationInfo(appliedType(StorageClass.tpe, List(tpar(1).tpeHK)), Nil, Nil))), tpar.head.tpeHK))
  lazy val marker_box2minibox =
    newPolyMethod(2, ConversionsObjectSymbol, newTermName("marker_box2minibox"), 0L)(
      tpar => (Some(List(tpar(0).tpeHK)), tpar.head.tpeHK withAnnotation AnnotationInfo(appliedType(StorageClass.tpe, List(tpar(1).tpeHK)), Nil, Nil)))

  // direct conversions
  lazy val x2minibox = Map[Symbol, Symbol](
      // Unit is incompatible with the Java-based runtime
      // UnitClass ->    definitions.getMember(ConversionsObjectSymbol, newTermName("unit2minibox")),
      BooleanClass -> definitions.getMember(ConversionsObjectSymbol, newTermName("boolean2minibox")),
      ByteClass ->    definitions.getMember(ConversionsObjectSymbol, newTermName("byte2minibox")),
      CharClass ->    definitions.getMember(ConversionsObjectSymbol, newTermName("char2minibox")),
      ShortClass ->   definitions.getMember(ConversionsObjectSymbol, newTermName("short2minibox")),
      IntClass ->     definitions.getMember(ConversionsObjectSymbol, newTermName("int2minibox")),
      LongClass ->    definitions.getMember(ConversionsObjectSymbol, newTermName("long2minibox")),
      DoubleClass ->  definitions.getMember(ConversionsObjectSymbol, newTermName("double2minibox")),
      FloatClass ->   definitions.getMember(ConversionsObjectSymbol, newTermName("float2minibox"))
    )
  lazy val minibox2x = Map[Symbol, Symbol](
      // Unit is incompatible with the Java-based runtime
      // UnitClass ->    definitions.getMember(ConversionsObjectSymbol, newTermName("minibox2unit")),
      BooleanClass -> definitions.getMember(ConversionsObjectSymbol, newTermName("minibox2boolean")),
      ByteClass ->    definitions.getMember(ConversionsObjectSymbol, newTermName("minibox2byte")),
      CharClass ->    definitions.getMember(ConversionsObjectSymbol, newTermName("minibox2char")),
      ShortClass ->   definitions.getMember(ConversionsObjectSymbol, newTermName("minibox2short")),
      IntClass ->     definitions.getMember(ConversionsObjectSymbol, newTermName("minibox2int")),
      LongClass ->    definitions.getMember(ConversionsObjectSymbol, newTermName("minibox2long")),
      DoubleClass ->  definitions.getMember(ConversionsObjectSymbol, newTermName("minibox2double")),
      FloatClass ->   definitions.getMember(ConversionsObjectSymbol, newTermName("minibox2float"))
    )

  lazy val standardTypeTagTrees = Map(
      UnitClass ->    Literal(Constant(UNIT)),
      BooleanClass -> Literal(Constant(BOOLEAN)),
      ByteClass ->    Literal(Constant(BYTE)),
      CharClass ->    Literal(Constant(CHAR)),
      ShortClass ->   Literal(Constant(SHORT)),
      IntClass ->     Literal(Constant(INT)),
      LongClass ->    Literal(Constant(LONG)),
      DoubleClass ->  Literal(Constant(DOUBLE)),
      FloatClass ->   Literal(Constant(FLOAT)),
      NothingClass -> Literal(Constant(REFERENCE))
    )

  // Manifest's newArray
  lazy val Manifest_newArray = definitions.getMember(FullManifestClass, newTermName("newArray"))

  def storageType(tparam: Symbol, spec: SpecInfo): Type = {
    // TODO: Allow multiple storage types, currently hardcoded for Long
    val storage = LongTpe
    tparam.tpe.withAnnotations(List(Annotation.apply(appliedType(StorageClass.tpe, List(storage)), Nil, ListMap.empty)))
  }

  lazy val opStorageClass = Map(
    mbarray_apply -> LongClass,
    mbarray_update -> LongClass,
    mbarray_length -> LongClass,
    mbarray_new -> LongClass,
    tag_hashCode -> LongClass,
    tag_toString -> LongClass,
    tag_== -> LongClass,
    notag_== -> LongClass,
    other_== -> LongClass
  )
}
