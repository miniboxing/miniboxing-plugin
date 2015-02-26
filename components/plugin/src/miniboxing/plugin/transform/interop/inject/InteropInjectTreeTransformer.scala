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
package inject

import scala.tools.nsc.transform.TypingTransformers

trait InteropInjectTreeTransformer extends TypingTransformers {
  this: InteropInjectComponent =>

  import global._
  import definitions._

  def newTransformer(unit: CompilationUnit) =
    if (flag_rewire_functionX_values) {
      if (delambdafySupport.isDelambdafyEnabled)
        // NOTE: The delambdafy transformation is incompatible with MiniboxedFunctionX-es.
        global.reporter.warning(unit.body.pos,
            "To gain the maximum performance, the miniboxing plugin represents functions (and closures) " +
            "as described at http://scala-miniboxing.org/example_functions.html. However, `-Ydelambdafy:method` " +
            "uses a different, incompatible and suboptimal function representation. Your program will still compile " +
            "but PLEASE DO NOT EXPECT THE BEST PEFORMANCE as long as you use delambdafication!")
      new InteropTreeInjector(unit)
    } else
      new Transformer { def apply(tree: Tree) = tree }

  class InteropTreeInjector(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match   {
      case TypeApply(sel, tpes) =>
        treeCopy.TypeApply(tree, transform(sel), tpes)
      case Template(parents, self, body) =>
         deriveTemplate(tree)(body => transformStats(body, currentOwner))
      case vd@ValDef(mods, name, tpt, rhs) if isDelambdafyParam(vd.symbol) =>
        treeCopy.ValDef(tree, mods, name, /* don't touch the */ tpt, transform(rhs))
      case tree: TypeTree =>
        val res = updatedType(tree.pos, tree.tpe)
        if (res eq tree.tpe)
          tree
        else
          TypeTree(res).setType(res)
      case _ =>
        super.transform(tree)
    }
  }
}