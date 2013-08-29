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

    override def transform(tree: Tree): Tree = afterMiniboxSpec(checkNoStorage(specTransform(tree)))

    def specTransform(tree: Tree): Tree = {
//      println("specTransform(" + tree + ")")
      val oldTpe = tree.tpe
      if (tree.hasSymbol)
        tree.symbol.info // force new info on the symbol

      tree match {
        case EmptyTree =>
          EmptyTree
        case _ =>
          val tree1 = super.transform(tree)
          val oldTpe = tree.tpe
          val newTpe = deepTransformation(tree.tpe)
          tree.tpe = newTpe
          assert(noStorageAnnot(tree.tpe), tree + "   <old>: " + oldTpe + "   <new>: " + newTpe)
//          if (oldTpe.hasAnnotation(StorageClass) && newTpe =:= LongTpe)
//            println("Need box2minibox conversion at: " + tree1)
//          else if (newTpe.hasAnnotation(StorageClass) && oldTpe =:= LongTpe)
//            println("Need minibox2box conversion at: " + tree1)
          println(tree1 + ": " + newTpe)
          tree1
      }
    }

    def checkNoStorage(tree: Tree) = {
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

  }
}
