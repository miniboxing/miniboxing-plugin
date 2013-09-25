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
    println(tree)
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

    var indent = 0

    def specTransform(tree0: Tree): Tree = {
//      val crtindent = indent
//      indent += 2
//      println(" " * crtindent + " --> " + tree0)

      // force new info on the symbol
      if (tree0.hasSymbol)
        tree0.symbol.info

      // transform subtrees of the tree
      val tree1 = super.transform(tree0)

      // minibox => box conversion
      val tree2 = tree match {
        case Apply(nfun, nargs) =>
          val aargs =
            for ((narg, tpe) <- nargs zip nfun.tpe.paramTypes) yield
              if (!(narg.tpe =:= LongTpe) && (tpe =:= LongTpe))
                convert_box_to_minibox(narg, currentMethod, currentClass)
              else if ((narg.tpe =:= LongTpe) && !(tpe =:= LongTpe))
                convert_minibox_to_box(narg, tpe, currentMethod, currentClass)
              else
                narg
          localTyper.typed(Apply(nfun, aargs))
        case _ =>
          tree1
      }

      // box => minibox conversion
      val tree3 = tree2 match {
        case EmptyTree =>
          EmptyTree
        case _ =>
          tree2.tpe = deepTransformation(tree2.tpe)

          // TODO: Getting type tags just to test whether the type
          // is miniboxed is a bit heavy-handed - maybe we can use
          // a lighter trick for this.
          lazy val tags = typeTagTrees(currentMethod, currentClass)
          val ntree =
            if (!tree2.isInstanceOf[TypeTree] &&
                tree2.tpe.typeSymbol.isTypeParameterOrSkolem &&
                tags.isDefinedAt(tree2.tpe.typeSymbol))
              localTyper.typed(convert_box_to_minibox(tree2, currentMethod, currentClass))
            else
              tree2

          assert(noStorageAnnot(ntree.tpe))
          ntree
      }

//      indent -= 2
//      println(" " * crtindent + " --> " + tree2)

      tree2
    }
  }
}
