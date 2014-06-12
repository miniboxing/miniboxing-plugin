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
//    * Eugene Burmako
//
package miniboxing.plugin
package transform
package inject

import scala.language.implicitConversions

trait TreeRewriters {
  this: MiniboxInjectComponent =>

  import global._

  abstract class TreeRewriter(unit: CompilationUnit) extends TypingTransformer(unit) {
    sealed trait Result
    case object Descend extends Result { override def toString = "<no rule>" }
    case class Single(tree: Tree) extends Result { override def toString = tree.toString }
    case class Multi(trees: List[Tree]) extends Result { override def toString = trees.toString }
    implicit def treeToResult(tree: Tree): Result = Single(tree)
    implicit def treesToResult(trees: List[Tree]): Result = Multi(trees)

    override def transform(tree: Tree): Tree = {
      rewrite(tree) match {
        case Descend => super.transform(tree)
        case Single(tree1) => tree1
        case Multi(trees) => localTyper.typed(Block(trees: _*))
      }
    }

    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      stats flatMap {
        case stat =>
          rewrite(stat) match {
            case Descend => List(super.transform(stat))
            case Single(stat1) => List(stat1)
            case Multi(stats1) => stats1
          }
      }
    }

    def rewrite(tree: Tree): Result
  }

  def classDefTreeFromSym(classSym: Symbol): Tree = {
    val parentsTree = classSym.info.parents map TypeTree
    val templateSym = classSym.newLocalDummy(classSym.pos)
    val templateTree = Template(parentsTree, noSelfType, List())
    ClassDef(classSym, templateTree.setSymbol(templateSym))
  }
}
