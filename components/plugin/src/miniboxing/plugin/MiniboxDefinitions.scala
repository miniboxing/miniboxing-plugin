package miniboxing.plugin

import scala.tools.nsc.plugins.PluginComponent

trait MiniboxDefinitions {
  this: PluginComponent =>

  import global._
  import definitions._
  import miniboxing.runtime.MiniboxConstants._

  lazy val MinispecClass = rootMirror.getRequiredClass("scala.miniboxed")
  lazy val StorageClass = rootMirror.getRequiredClass("miniboxing.plugin.storage")

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
  lazy val minibox2box       =  definitions.getMember(ConversionsObjectSymbol, newTermName("minibox2box"))
  lazy val box2minibox       =  definitions.getMember(ConversionsObjectSymbol, newTermName("box2minibox_tt"))
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
}
