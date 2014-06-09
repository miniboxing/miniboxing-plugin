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
//
package miniboxing.plugin
package transform
package commit

import scala.tools.nsc.transform.InfoTransform

trait MiniboxCommitInfoTransformer extends InfoTransform {
  this: MiniboxCommitComponent =>

  import global._
  import definitions._
  import minibox._

  override def transformInfo(sym: Symbol, tpe: Type): Type = {
    val tpe2 = deepTransformation.transform(sym, tpe)
//    if (!(tpe =:= tpe2))
//      println(sym + "  old: " + tpe + "  new: " + tpe2)
    tpe2
  }

  object deepTransformation extends TypeMap {

    var symbol: Symbol = NoSymbol

    def apply(tpe: Type) = transform(NoSymbol, tpe)
    def transform(sym: Symbol, tpe: Type): Type = {
      symbol = sym
      val res = mapOver(tpe)
      symbol = NoSymbol
      res
    }

    override def mapOver(tpe: Type): Type = tpe match {
      case tpe if tpe.annotations.exists(ann => ann.tpe.typeSymbol == StorageClass) =>
        val annots = tpe.annotations.filter(ann => ann.tpe.typeSymbol == StorageClass)
        if (annots.length != 1)
          global.reporter.error(symbol.pos, s"Multiple annotations found for $symbol: ${beforeMiniboxCommit(symbol.tpe)}")
        val annot = annots.head
        annot.tpe.typeArgs(0)
      case _ =>
        super.mapOver(tpe)
    }
  }
}
