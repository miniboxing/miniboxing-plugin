package miniboxing.plugin
package metadata

trait MiniboxMetadataAddons {
  self: MiniboxDuplComponent =>

  import global._
  import definitions._
  import scala.collection.immutable

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
}
