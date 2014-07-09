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
  case class  Miniboxed(repr: Symbol) extends SpecInfo
  case object Boxed                   extends SpecInfo


  // Symbols:

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

  // artificially created marker methods:

  def withStorage(tpar: Symbol, repr: Symbol) =
    tpar.tpeHK withAnnotation AnnotationInfo(appliedType(PolyType(StorageClass.typeParams, StorageClass.tpe), List(repr.tpeHK)), Nil, Nil)

  //   def marker_minibox2box[T, St](t: T @storage[St]): T
  lazy val marker_minibox2box =
    newPolyMethod(2, ConversionsObjectSymbol, newTermName("marker_minibox2box"), 0L)(
      tpar => (Some(List(withStorage(tpar(0), tpar(1)))), tpar(0).tpeHK))
  //   def marker_box2minibox[T, St](t: T): T @storage[St]
  lazy val marker_box2minibox =
    newPolyMethod(2, ConversionsObjectSymbol, newTermName("marker_box2minibox"), 0L)(
      tpar => (Some(List(tpar(0).tpeHK)), withStorage(tpar(0), tpar(1))))
  //   def marker_minibox2minibox[T, St1, St2](t: T @storage[St1]): T @storage[St2]
  lazy val marker_minibox2minibox =
    newPolyMethod(3, ConversionsObjectSymbol, newTermName("marker_minibox2minibox"), 0L)(
      tpar => (Some(List(withStorage(tpar(0), tpar(1)))), withStorage(tpar(0), tpar(2))))


  // Library:

  // array ops
  lazy val MiniboxArrayObjectSymbol = rootMirror.getRequiredModule("miniboxing.runtime.MiniboxArray")
  trait array {
    def owner: Symbol
    lazy val mbarray_update  = definitions.getMember(owner, newTermName("mbarray_update_minibox"))
    lazy val mbarray_apply   = definitions.getMember(owner, newTermName("mbarray_apply_minibox"))
  }

  object array_1way        extends array { lazy val owner  = MiniboxArrayObjectSymbol }
  object array_2way_long   extends array { lazy val owner  = rootMirror.getRequiredModule("miniboxing.runtime.MiniboxArrayLong") }
  object array_2way_double extends array { lazy val owner  = rootMirror.getRequiredModule("miniboxing.runtime.MiniboxArrayDouble") }

  def array(repr: Symbol): array = repr match {
    case _ if !flag_two_way => array_1way
    case LongClass          => array_2way_long
    case DoubleClass        => array_2way_double
  }

  lazy val mbarray_length  = definitions.getMember(MiniboxArrayObjectSymbol, newTermName("mbarray_length"))
  lazy val mbarray_new     = definitions.getMember(MiniboxArrayObjectSymbol, newTermName("mbarray_new"))
  def mbarray_update(repr: Symbol) = array(repr).mbarray_update
  def mbarray_apply(repr: Symbol)  = array(repr).mbarray_apply

  // Any ops
  trait ops {
    def owner: Symbol
    lazy val tag_hashCode = definitions.getMember(owner, newTermName("mboxed_hashCode"))
    lazy val other_==     = definitions.getMember(owner, newTermName("mboxed_eqeq_other"))
    lazy val tag_==       = definitions.getMember(owner, newTermName("mboxed_eqeq_tag"))
    lazy val notag_==     = definitions.getMember(owner, newTermName("mboxed_eqeq_notag"))
    lazy val tag_toString = definitions.getMember(owner, newTermName("mboxed_toString"))
  }
  object ops_1way        extends ops { lazy val owner = rootMirror.getRequiredModule("miniboxing.runtime.MiniboxDispatch") }
  object ops_2way_long   extends ops { lazy val owner = rootMirror.getRequiredModule("miniboxing.runtime.MiniboxDispatchLong") }
  object ops_2way_double extends ops { lazy val owner = rootMirror.getRequiredModule("miniboxing.runtime.MiniboxDispatchDouble")}

  def ops(repr: Symbol): ops = repr match {
    case _ if !flag_two_way => ops_1way
    case LongClass          => ops_2way_long
    case DoubleClass        => ops_2way_double
  }

  def tag_hashCode(repr: Symbol) = ops(repr).tag_hashCode
  def other_==(repr: Symbol)     = ops(repr).other_==
  def tag_==(repr: Symbol)       = ops(repr).tag_==
  def notag_==(repr: Symbol)     = ops(repr).notag_==
  def tag_toString(repr: Symbol) = ops(repr).tag_toString

  // conversions
  lazy val ConversionsObjectSymbol = rootMirror.getRequiredModule("miniboxing.runtime.MiniboxConversions")
  lazy val ConversionsObjectLongSymbol = rootMirror.getRequiredModule("miniboxing.runtime.MiniboxConversionsLong")
  lazy val ConversionsObjectDoubleSymbol = rootMirror.getRequiredModule("miniboxing.runtime.MiniboxConversionsDouble")
  trait convs {
    def owner: Symbol
    lazy val box2minibox = definitions.getMember(owner, newTermName("box2minibox_tt"))
    lazy val minibox2box = definitions.getMember(owner, newTermName("minibox2box"))
    def minibox2x: Map[Symbol, Symbol]
    def x2minibox: Map[Symbol, Symbol]
  }
  def x2minibox_long: Map[Symbol, Symbol] =
    Map(
      CharClass ->    definitions.getMember(ConversionsObjectSymbol, newTermName("char2minibox")),
      IntClass ->     definitions.getMember(ConversionsObjectSymbol, newTermName("int2minibox")),
      LongClass ->    definitions.getMember(ConversionsObjectSymbol, newTermName("long2minibox"))
    )
  def minibox2x_long: Map[Symbol, Symbol] =
    Map(
      CharClass ->    definitions.getMember(ConversionsObjectSymbol, newTermName("minibox2char")),
      IntClass ->     definitions.getMember(ConversionsObjectSymbol, newTermName("minibox2int")),
      LongClass ->    definitions.getMember(ConversionsObjectSymbol, newTermName("minibox2long"))
    )
  def x2minibox_double(owner: Symbol): Map[Symbol, Symbol] =
    Map(
      DoubleClass ->  definitions.getMember(owner, newTermName("double2minibox")),
      FloatClass ->   definitions.getMember(owner, newTermName("float2minibox"))
    )
  def minibox2x_double(owner: Symbol): Map[Symbol, Symbol] =
    Map(
      DoubleClass ->  definitions.getMember(owner, newTermName("minibox2double")),
      FloatClass ->   definitions.getMember(owner, newTermName("minibox2float"))
    )

  object convs_1way extends convs {
    def owner = ConversionsObjectSymbol
    lazy val minibox2x = minibox2x_long ++ minibox2x_double(owner)
    lazy val x2minibox = x2minibox_long ++ x2minibox_double(owner)
  }
  object convs_2way_long extends convs {
    def owner = ConversionsObjectLongSymbol
    lazy val minibox2x = minibox2x_long
    lazy val x2minibox = x2minibox_long
  }
  object convs_2way_double extends convs {
    def owner = ConversionsObjectDoubleSymbol
    lazy val minibox2x = minibox2x_double(owner)
    lazy val x2minibox = x2minibox_double(owner)
  }

  def convs(repr: Symbol): convs =  repr match {
    case _ if !flag_two_way => convs_1way
    case LongClass          => convs_2way_long
    case DoubleClass        => convs_2way_double
  }

  def minibox2box(repr: Symbol) = convs(repr).minibox2box
  def box2minibox(repr: Symbol) = convs(repr).box2minibox
  def minibox2x(repr: Symbol) = convs(repr).minibox2x
  def x2minibox(repr: Symbol) = convs(repr).x2minibox
  lazy val unreachableConversion = definitions.getMember(ConversionsObjectSymbol, newTermName("unreachableConversion"))

  // direct conversions

  lazy val standardTypeTagTrees = Map(
      CharClass ->    Literal(Constant(CHAR)),
      IntClass ->     Literal(Constant(INT)),
      LongClass ->    Literal(Constant(LONG)),
      DoubleClass ->  Literal(Constant(DOUBLE)),
      FloatClass ->   Literal(Constant(FLOAT)),
      NothingClass -> Literal(Constant(REFERENCE))
    )

  // Manifest's newArray
  lazy val Manifest_newArray = definitions.getMember(FullManifestClass, newTermName("newArray"))

  def storageType(tparam: Symbol, spec: SpecInfo): Type = {
    val Miniboxed(repr) = spec
    withStorage(tparam, repr)
  }

  // filled in from outside
  def flag_two_way: Boolean
}
