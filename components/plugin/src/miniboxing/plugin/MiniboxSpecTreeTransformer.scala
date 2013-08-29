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

  override def newTransformer(unit: CompilationUnit): Transformer = new MiniboxTreeTransformer(unit)

  class MiniboxTreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree: Tree): Tree = afterMiniboxSpec(specTransform(tree))

    def specTransform(tree: Tree): Tree = {
//      println("specTransform(" + tree + ")")
      val oldTpe = tree.tpe
      if (tree.hasSymbol)
        tree.symbol.info // force new info on the symbol

      tree match {
        case EmptyTree =>
          EmptyTree
        case _: TypeTree =>
          if (tree.tpe.hasAnnotation(StorageClass))
            tree.tpe = LongTpe
          tree
        case _ =>
          val tree1 = super.transform(tree)
          if (tree1.tpe.hasAnnotation(StorageClass))
            tree1.tpe = LongTpe
          val newTpe = tree.tpe
//          if (oldTpe.hasAnnotation(StorageClass) && newTpe =:= LongTpe)
//            println("Need box2minibox conversion at: " + tree1)
//          else if (newTpe.hasAnnotation(StorageClass) && oldTpe =:= LongTpe)
//            println("Need minibox2box conversion at: " + tree1)
          tree1
      }
    }
  }
}
