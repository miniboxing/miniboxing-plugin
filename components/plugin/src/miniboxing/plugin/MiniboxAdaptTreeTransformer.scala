package miniboxing.plugin

import scala.tools.nsc._
import scala.tools.nsc.typechecker._
import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.transform.TypingTransformers
import scala.collection.{ mutable, immutable }
import scala.collection.mutable.Set
import scala.collection.mutable.{ Map => MMap }

trait MiniboxAdaptTreeTransformer extends TypingTransformers {
  self: MiniboxAdaptComponent =>

  import global._

  object TreeAdapter extends {
    val global: MiniboxAdaptTreeTransformer.this.global.type = MiniboxAdaptTreeTransformer.this.global
  } with TreeAdapter

  def newTransformer(unit: CompilationUnit): Transformer = new TypingTransformer(unit) {
    override def transform(tree: Tree): Tree =
      TreeAdapter.adapted(localTyper.context1.asInstanceOf[TreeAdapter.Context], tree)
  }

  abstract class TreeAdapter extends Analyzer {
    import global._
    import definitions.{ AnyRefClass, AnyValClass }
    val StorageClass = minibox.StorageClass.asInstanceOf[Symbol]

    def adapted(context: Context, tree: Tree): Tree =
      (new Adapter(context)).typed(tree)

    class Adapter(_context: Context) extends Typer(_context) {
      override def typed(tree: Tree, mode: Int, pt: Type): Tree = {
        val oldTpe = tree.tpe
        tree.tpe = null
        super.typed(tree, mode, WildcardType)
        //if (tree.tpe.hasAnnotation(StorageClass) != oldTpe.hasAnnotation(StorageClass))
        println(tree)
        println("new = " + tree.tpe + " vs old: " + oldTpe)
        tree
      }
    }
  }
}

