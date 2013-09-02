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

      // minibox => box conversion
      val tree1 = tree match {
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
          localTyper.typed(Apply(nfun, aargs))
        case _ =>
          tree
      }

      // box => minibox conversion
      val tree2 = tree1 match {
        case EmptyTree =>
          EmptyTree
        case _ =>
          val tree1 = super.transform(tree)
          val oldTpe = tree.tpe
          val newTpe = deepTransformation(tree.tpe)
          tree1.tpe = newTpe

          // TODO: Getting type tags just to test whether the type
          // is miniboxed is a bit heavy-handed - maybe we can use
          // a lighter trick for this.
          lazy val tags = typeTagTrees(currentMethod, currentClass)
          val tree2 =
            if (!tree1.isInstanceOf[TypeTree] &&
                newTpe.typeSymbol.isTypeParameterOrSkolem &&
                tags.isDefinedAt(newTpe.typeSymbol)) {
              println
              println("Converting box to minibox: " + tree1 + ": " + showRaw(newTpe))
              localTyper.typed(convert_box_to_minibox(tree1, currentMethod, currentClass))
            } else
              tree1

          assert(noStorageAnnot(tree2.tpe), tree + "   <old>: " + oldTpe + "   <new>: " + newTpe)
          tree2
      }

      tree2
    }
  }
}
