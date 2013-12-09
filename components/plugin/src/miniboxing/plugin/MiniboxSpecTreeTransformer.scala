package miniboxing.plugin

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.TypingTransformers
import scala.collection.mutable.Set
import scala.tools.nsc.typechecker._
import scala.collection.mutable.{ Map => MMap }

trait MiniboxPostTreeTransformer extends TypingTransformers {
  self: MiniboxSpecComponent =>

  import global._
  import definitions._
  import minibox._
  import Flags._
  import typer.{ typed, atOwner }

  override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    val specTrans = new MiniboxTreeTransformer(unit)
    override def transform(tree: Tree): Tree =
      afterMiniboxSpec(checkNoStorage(specTrans.transform(tree)))
  }

  def checkNoStorage(tree: Tree) = {
    //println(tree)
    for (t <- tree)
      assert(noStorageAnnot(t.tpe), t + ": " + t.tpe)
    tree
  }

  def noStorageAnnot(t: Type): Boolean = {
    var hasStorage = false
    new TypeMap {
      def apply(tp: Type): Type = mapOver(tp)
      override def mapOver(tp: Type): Type = tp match {
        case _ if tp hasAnnotation(StorageClass) =>
          hasStorage = true
          tp
        case _ =>
          super.mapOver(tp)
      }
    }.apply(t)

    !hasStorage
  }

  abstract sealed class Constraint
  case object Miniboxed extends Constraint
  case object Boxed extends Constraint
  case object NoConstraint extends Constraint

  class MiniboxTreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree0: Tree): Tree = {
      val oldTpe = tree0.tpe
      val newTpe = deepTransformation(oldTpe)
//      println(oldTpe + " ==> " + newTpe)

      // force new info on the symbol
      if (tree0.hasSymbol)
        tree0.symbol.info

      val tree1 =
        tree0 match {
          case Apply(TypeApply(fun, List(targ)), List(tree)) if fun.symbol == marker_box2minibox =>
            val tags = minibox.typeTagTrees(currentOwner)
            localTyper.typed(gen.mkMethodCall(box2minibox, List(targ.tpe), List(transform(tree), tags(targ.tpe.typeSymbol))))
          case Apply(TypeApply(fun, List(targ)), List(tree)) if fun.symbol == marker_minibox2box =>
            val tags = minibox.typeTagTrees(currentOwner)
            localTyper.typed(gen.mkMethodCall(minibox2box, List(targ.tpe), List(transform(tree), tags(targ.tpe.typeSymbol))))
          case _ =>
            super.transform(tree0)
        }

      tree1.setType(newTpe)
    }
  }
}
