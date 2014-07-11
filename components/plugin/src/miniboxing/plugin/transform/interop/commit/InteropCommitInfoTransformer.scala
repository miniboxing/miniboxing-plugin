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
package miniboxing
package plugin
package transform
package interop
package commit

import scala.tools.nsc.transform.InfoTransform
import miniboxing.runtime.MiniboxedFunction0

trait InteropCommitInfoTransformer extends InfoTransform {
  self: InteropCommitComponent =>

  import global._
  import definitions._
  import interop._

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
      case tpe if tpe.annotations.exists(ann => ann.tpe.typeSymbol == mbFunctionClass) =>
        val annots = tpe.annotations.filter(ann => ann.tpe.typeSymbol == mbFunctionClass)
        if (annots.length != 1)
          global.reporter.error(symbol.pos, s"Multiple annotations found for $symbol: ${beforeInteropCommit(symbol.tpe)}")
        val annot = annots.head
        // miniboxed functions ftw!
        val mbFun =
          tpe.withoutAnnotations match {
            case TypeRef(_, Function0Class, tpes) =>  appliedType(MiniboxedFunction0PolyTpe, tpes)
            case TypeRef(_, Function1Class, tpes) =>  appliedType(MiniboxedFunction1PolyTpe, tpes)
            case TypeRef(_, Function2Class, tpes) =>  appliedType(MiniboxedFunction2PolyTpe, tpes)
            case _ =>
//              This occurs for the marker_fun2mbfun and the reverse
//              println("unknown type: " + tpe + "   " + tpe.typeSymbol.ownerChain)
              tpe
          }
        // keep all other annotations
        mbFun.withAnnotations(tpe.annotations.filterNot(_.tpe.typeSymbol == mbFunctionClass))
      case _ =>
        super.mapOver(tpe)
    }
  }
}