package miniboxing.plugin
package transform
package dupl

import miniboxing.plugin.MiniboxDuplComponent
import scala.language.implicitConversions

trait TreeRewriters {
  this: MiniboxDuplComponent =>

  import global._

  abstract class TreeRewriter(unit: CompilationUnit) extends TypingTransformer(unit) {
    sealed trait Result
    case class Single(tree: Tree) extends Result { override def toString = tree.toString }
    case class Multi(trees: List[Tree]) extends Result { override def toString = trees.toString }
    implicit def treeToResult(tree: Tree): Result = Single(tree)
    implicit def treesToResult(trees: List[Tree]): Result = Multi(trees)

    override def transform(tree: Tree): Tree =
      rewrite(tree)
//    {
//      rewrite(tree) match {
//        case Single(tree1) => tree1
//        case Multi(trees) => typer.typed(Block(trees.map(super.transform _): _*))
//      }
//    }

//    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
//      stats flatMap {
//        case stat =>
//          rewrite(stat) match {
//            case Single(stat1) => List(stat1)
//            case Multi(stats1) => stats1
//          }
//      }
//    }

    def rewrite(tree: Tree): Tree
  }
}
