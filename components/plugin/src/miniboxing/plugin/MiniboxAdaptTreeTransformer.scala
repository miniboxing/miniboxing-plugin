package miniboxing.plugin

import scala.tools.nsc._
import scala.tools.nsc.typechecker._
import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.transform.TypingTransformers
import scala.collection.{ mutable, immutable }
import scala.collection.mutable.Set
import scala.collection.mutable.{ Map => MMap }
import util.returning
import scala.collection.mutable.ListBuffer

trait MiniboxAdaptTreeTransformer extends TypingTransformers {
  self: MiniboxAdaptComponent =>

  import minibox._
  import global._

  class AdaptPhase(prev: Phase) extends StdPhase(prev) {
    override def name = MiniboxAdaptTreeTransformer.this.phaseName
    def apply(unit: CompilationUnit): Unit = {
      object TreeChecker extends {
        val global: MiniboxAdaptTreeTransformer.this.global.type = MiniboxAdaptTreeTransformer.this.global
      } with TreeCheckers

      TreeChecker.check(unit)
      //newTransformer(unit).transformUnit(unit)
    }
  }

  abstract class TreeCheckers extends Analyzer {
    import global._

    private def wrap[T](msg: => Any)(body: => Unit) {
      try body
      catch { case x: Throwable =>
        Console.println("Caught " + x)
        Console.println(msg)
        x.printStackTrace
      }
    }

    def check(unit: CompilationUnit) {
      val context = rootContext(unit)
      val checker = new TreeChecker(context)
      checker.typed(unit.body)
    }

    override def newTyper(context: Context): Typer = new TreeChecker(context)

    class TreeChecker(context0: Context) extends Typer(context0) {
      override protected def finishMethodSynthesis(templ: Template, clazz: Symbol, context: Context): Template =
        templ

      override def typed(tree: Tree, mode: Int, pt: Type): Tree = {
        returning(tree) {
          case EmptyTree | TypeTree() => ()
          case _ if tree.tpe != null  =>
//            println("TREE: " + tree)
            val oldTpe = tree.tpe
            tree.tpe = null
            super.typed(tree, mode, WildcardType) match {
              case _: Literal     => ()
              case x if x ne tree => ???
              case _              => ()
            }
            val newTpe = tree.tpe
            if (oldTpe.hasAnnotation(StorageClass.asInstanceOf[Symbol]) != newTpe.hasAnnotation(StorageClass.asInstanceOf[Symbol]))
              println(s"Mismatch: $oldTpe vs $newTpe:\n$tree\n")
          case _ => ()
        }
      }
    }
  }
}

