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
package inject

import scala.tools.nsc.transform.TypingTransformers

trait InteropInjectTreeTransformer extends TypingTransformers {
  this: InteropInjectComponent =>

  import global._
  import definitions._

  def newTransformer(unit: CompilationUnit) =
    new InteropTreeInjector(unit)

  class InteropTreeInjector(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match   {
      case TypeApply(sel, tpes) =>
        treeCopy.TypeApply(tree, transform(sel), tpes)
      case tree: TypeTree =>
        val res = updatedType(tree.tpe)
        if (res eq tree.tpe)
          tree
        else
          TypeTree(res)
      case _ =>
        super.transform(tree)
    }
  }
}