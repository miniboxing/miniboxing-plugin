package miniboxing.plugin

import scala.tools.nsc.plugins.PluginComponent
import scala.reflect.internal.Flags
import scala.collection.immutable.ListMap

trait MiniboxDefinitions {
  this: PluginComponent =>

  import global._
  import Flags._
  import definitions._
  import miniboxing.runtime.MiniboxConstants._

  lazy val MinispecClass = rootMirror.getRequiredClass("scala.miniboxed")
  lazy val StorageClass = rootMirror.getRequiredClass("scala.storage")

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
  lazy val marker_minibox2box =
    newPolyMethod(1, ConversionsObjectSymbol, newTermName("marker_minibox2box"), 0L)(tpar => (Some(List(tpar.head.tpeHK withAnnotation AnnotationInfo(StorageClass.tpe, Nil, Nil))), tpar.head.tpeHK))
  lazy val marker_box2minibox =
    newPolyMethod(1, ConversionsObjectSymbol, newTermName("marker_box2minibox"), 0L)(tpar => (Some(List(tpar.head.tpeHK)), tpar.head.tpeHK withAnnotation AnnotationInfo(StorageClass.tpe, Nil, Nil)))

  // direct conversions
  lazy val x2minibox = Map(
      UNIT ->    definitions.getMember(ConversionsObjectSymbol, newTermName("UnitToMinibox")),
      BOOLEAN -> definitions.getMember(ConversionsObjectSymbol, newTermName("BooleanToMinibox")),
      BYTE ->    definitions.getMember(ConversionsObjectSymbol, newTermName("ByteToMinibox")),
      CHAR ->    definitions.getMember(ConversionsObjectSymbol, newTermName("CharToMinibox")),
      SHORT ->   definitions.getMember(ConversionsObjectSymbol, newTermName("ShortToMinibox")),
      INT ->     definitions.getMember(ConversionsObjectSymbol, newTermName("IntToMinibox")),
      LONG ->    definitions.getMember(ConversionsObjectSymbol, newTermName("LongToMinibox")),
      DOUBLE ->  definitions.getMember(ConversionsObjectSymbol, newTermName("DoubleToMinibox")),
      FLOAT ->   definitions.getMember(ConversionsObjectSymbol, newTermName("FloatToMinibox"))
    )
  lazy val minibox2x = Map(
      UNIT ->    definitions.getMember(ConversionsObjectSymbol, newTermName("MiniboxToUnit")),
      BOOLEAN -> definitions.getMember(ConversionsObjectSymbol, newTermName("MiniboxToBoolean")),
      BYTE ->    definitions.getMember(ConversionsObjectSymbol, newTermName("MiniboxToByte")),
      CHAR ->    definitions.getMember(ConversionsObjectSymbol, newTermName("MiniboxToChar")),
      SHORT ->   definitions.getMember(ConversionsObjectSymbol, newTermName("MiniboxToShort")),
      INT ->     definitions.getMember(ConversionsObjectSymbol, newTermName("MiniboxToInt")),
      LONG ->    definitions.getMember(ConversionsObjectSymbol, newTermName("MiniboxToLong")),
      DOUBLE ->  definitions.getMember(ConversionsObjectSymbol, newTermName("MiniboxToDouble")),
      FLOAT ->   definitions.getMember(ConversionsObjectSymbol, newTermName("MiniboxToFloat"))
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

  // TODO: This will also take the storage type
  def storageType(tparam: Symbol): Type =
    tparam.tpe.withAnnotations(List(Annotation.apply(StorageClass.tpe, Nil, ListMap.empty)))

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

//  def isRuntimeSymbol(sym: Symbol) =
//    opStorageClass.isDefinedAt(sym)
//
//  def transformRuntimeSymbolInfo(sym: Symbol, info: Type): Type = {
//    val tp = sym.newTypeSymbol(newTypeName("$T$"), sym.pos, 0L)
//    tp.info = TypeBounds(NothingTpe, AnyTpe)
//    val tt = storageType(deriveFreshSkolems(List(tp)).head)
//    val res = info.substituteSymbols(opStorageClass(sym) :: Nil, tp :: Nil)
//    println(sym + ": " + info + " ==> " + res)
//    res
//  }
}
