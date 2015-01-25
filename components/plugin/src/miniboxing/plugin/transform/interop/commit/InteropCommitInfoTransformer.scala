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
//    * Vlad Ureche
//
package miniboxing
package plugin
package transform
package interop
package commit

import scala.tools.nsc.transform.InfoTransform
import runtime.MiniboxedFunction0

trait InteropCommitInfoTransformer extends InfoTransform {
  self: InteropCommitComponent =>

  import global._
  import definitions._
  import interop._

  override def transformInfo(sym: Symbol, tpe: Type): Type = {

    import AnonymousFunctionSupport._

    val tpe1 = deepTransformation.transform(sym, tpe)
    val tpe2 =
      tpe1 match {
        case ClassInfoType(parents, decls, _) if sym.isAnonymousFunction &&
                                                 isTypicalParentList(parents) &&
                                                 isTypicalDeclarationList(decls.toList) &&
                                                 flag_rewire_functionX_repres =>
          // Desugared anonymous
          val parents2 = tweakedParents(parents)
          transformedAnonFunctions += sym
          ClassInfoType(parents2, decls, sym)
        case _ =>
          tpe1
      }
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