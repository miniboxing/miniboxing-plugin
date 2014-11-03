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
//    * Eugene Burmako
//
package miniboxing.plugin
package metadata

trait MiniboxMetadataAddons {
  self: MiniboxInjectComponent =>

  import global._
  import definitions._
  import scala.collection.immutable

  // Thanks to @xeno-by :)
  implicit class RichSym(sym: Symbol) {
    def getMiniboxedTypeParameters: List[Symbol] =
      sym.typeParams.filter((s: Symbol) => s.isMiniboxAnnotated)
    def hasMiniboxedTypeParameters: Boolean = {
      val existsMbox = sym.typeParams.exists((s: Symbol) => s.isMiniboxAnnotated)
      val existsSpec = sym.typeParams.exists((s: Symbol) => s hasAnnotation SpecializedClass)

      // #117: you can't mix @specialized and @miniboxed!
      if (existsMbox && existsSpec)
        global.reporter.error(sym.pos, s"You can't mix @specialized and @miniboxed in the same ${sym.kindString}. Use only @miniboxing!")

      existsMbox && !existsSpec
    }
    def isMiniboxAnnotated: Boolean = {
      beforeMiniboxInject(sym.info) // make sure the annotation hijacker updated it
      sym hasAnnotation MinispecClass
    }
    def isField = sym.isValue && !sym.isMethod
  }
}
