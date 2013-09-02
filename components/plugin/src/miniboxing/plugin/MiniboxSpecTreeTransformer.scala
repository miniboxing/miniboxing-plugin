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

  class MiniboxTreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree: Tree): Tree = specTransform(tree)

    def specTransform(tree: Tree): Tree = {
      val oldTpe = tree.tpe
      if (tree.hasSymbol)
        tree.symbol.info // force new info on the symbol

      tree match {
        case EmptyTree =>
          EmptyTree
        case Apply(fun, args) =>
          val nfun = specTransform(fun)
          val nargs = args.map(specTransform)
          val aargs =
            for ((narg, tpe) <- nargs zip fun.tpe.paramTypes) yield
              if (!(narg.tpe =:= LongTpe) && (tpe =:= LongTpe))
                convert_box_to_minibox(narg, currentMethod, currentClass)
              else if ((narg.tpe =:= LongTpe) && !(tpe =:= LongTpe))
                convert_minibox_to_box(narg, tpe, currentMethod, currentClass)
              else
                narg
//          println()
//          println(tree + ": " + tree.tpe)
//          println("after: " + fun.tpe)
//          println("args: " + args.map(_.tpe).mkString(", "))
//          println(aargs)
          localTyper.typed(Apply(nfun, aargs))
        case _ =>
          val tree1 = super.transform(tree)
          val oldTpe = tree.tpe
          val newTpe = deepTransformation(tree.tpe)
          tree1.tpe = newTpe
//          if (tree.isInstanceOf[Block]) {
//            println
//            println(tree)
//            println(tree1.tpe)
//          }
          assert(noStorageAnnot(tree1.tpe), tree + "   <old>: " + oldTpe + "   <new>: " + newTpe)
          // TODO: If the current type is Tsp (not Tsp @storage), and Tsp should be miniboxed,
          // we should use box2minibox to convert it to the miniboxed representation.
          tree1
      }
    }
  }
}
