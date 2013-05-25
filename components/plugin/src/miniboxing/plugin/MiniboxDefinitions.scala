package miniboxing.plugin

trait MiniboxDefinitions {
  this: MiniboxInfoTransformation =>

  import global._

  lazy val TagDipatchObjectSymbol = rootMirror.getRequiredModule("miniboxing.runtime.MiniboxTypeTagDispatch")
  lazy val array_update = definitions.getMember(TagDipatchObjectSymbol, newTermName("array_update"))
  lazy val array_apply =  definitions.getMember(TagDipatchObjectSymbol, newTermName("array_apply"))
  lazy val array_length = definitions.getMember(TagDipatchObjectSymbol, newTermName("array_length"))

  lazy val tag_hashCode = definitions.getMember(TagDipatchObjectSymbol, newTermName("hashCode"))
  lazy val tag_## =       definitions.getMember(TagDipatchObjectSymbol, newTermName("hashhash"))
  lazy val tag_== =       definitions.getMember(TagDipatchObjectSymbol, newTermName("eqeq"))

  lazy val ConversionsObjectSymbol = rootMirror.getRequiredModule("miniboxing.runtime.MiniboxConversions")
  lazy val minibox2box =  definitions.getMember(ConversionsObjectSymbol, newTermName("minibox2box"))
  lazy val box2minibox =  definitions.getMember(ConversionsObjectSymbol, newTermName("box2minibox"))

  lazy val MiniboxArrayObjectSymbol = rootMirror.getRequiredModule("miniboxing.runtime.MiniboxArray")
  lazy val newArray =     definitions.getMember(MiniboxArrayObjectSymbol, newTermName("newArray"))
  lazy val internal_newArray = definitions.getMember(MiniboxArrayObjectSymbol, newTermName("internal_newArray"))

}
