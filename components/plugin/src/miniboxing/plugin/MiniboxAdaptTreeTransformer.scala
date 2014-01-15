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
    override def checkable = false
    def apply(unit: CompilationUnit): Unit = {

      object TreeAdapter extends {
        val global: MiniboxAdaptTreeTransformer.this.global.type = MiniboxAdaptTreeTransformer.this.global
      } with TreeAdapters

      // boil frog, boil!
      global.addAnnotationChecker(StorageAnnotationChecker)

      val tree = TreeAdapter.adapt(unit)
      tree.foreach(node => assert(node.tpe != null, node))
    }
  }

  abstract class TreeAdapters extends Analyzer {
//    val global: Global
//    val minibox: MiniboxAdaptTreeTransformer { val global: TreeAdapters.this.global.type }
//
//    import global._
//    import minibox._
    import global._

    var indent = 0

    private def wrap[T](msg: => Any)(body: => Unit) {
      try body
      catch { case x: Throwable =>
        Console.println("Caught " + x)
        Console.println(msg)
        x.printStackTrace
      }
    }

    def adapt(unit: CompilationUnit): Tree = {
      val context = rootContext(unit)
      val checker = new TreeAdapter(context)
      unit.body = checker.typed(unit.body)
      unit.body
    }

    var normalTyper = false

    def withNormalTyper[T](context: Context)(f: Typer => T): T = {
      val normalTyper0 = normalTyper
      normalTyper = true
      val res = f(newTyper(context))
      normalTyper = normalTyper0
      res
    }

    override def newTyper(context: Context): Typer =
      if (normalTyper) {
        super.newTyper(context)
      } else {
        new TreeAdapter(context)
      }

    def adaptdbg(ind: Int, msg: String): Unit = {
//      println("  " * ind + msg)
    }

    class TreeAdapter(context0: Context) extends Typer(context0) {
      override protected def finishMethodSynthesis(templ: Template, clazz: Symbol, context: Context): Template =
        templ

      def supertyped(tree: Tree, mode: Int, pt: Type): Tree =
        super.typed(tree, mode, pt)

      override def typed(tree: Tree, mode: Int, pt: Type): Tree = {
        val ind = indent
        indent += 1
        adaptdbg(ind, " <== " + tree + ": " + showRaw(pt, true, true, false, false))
        val res = tree match {
          case EmptyTree | TypeTree() =>
            super.typed(tree, mode, pt)
          case Select(mbox, mth) if mbox.tpe != null && mbox.tpe.dealiasWiden.hasAnnotation(StorageClass.asInstanceOf[Symbol]) =>
            val box = gen.mkMethodCall(marker_minibox2box.asInstanceOf[Symbol], List(mbox.tpe.dealiasWiden.typeSymbol.tpeHK), List(mbox))
            val sel = Select(box, mth)
            super.typed(sel, mode, pt)

          // IMPORTANT NOTE: ErrorType should be allowed to bubble up, since there will certainly be
          // a silent_.typed ready to catch the error and perform the correct rewriting upstream.
          case _ if tree.tpe != null && tree.tpe != ErrorType =>
            //adaptdbg(ind, "TREE: " + tree + " pt = " + pt)
            // **TODO: Isn't def adapt a better place to hook into the typer?**
            val oldTree = tree.duplicate
            val oldTpe = tree.tpe
            tree.tpe = null
            val res: Tree = silent(_.typed(tree, mode, pt)) match {
              case SilentTypeError(err) =>
                val newTpe = pt
                val hAnnot1 = oldTpe.dealiasWiden.hasAnnotation(StorageClass.asInstanceOf[Symbol])
                val hAnnot2 = newTpe.dealiasWiden.hasAnnotation(StorageClass.asInstanceOf[Symbol])
                adaptdbg(ind, "adapting:")
                adaptdbg(ind, s"$tree ::: ${oldTpe.dealiasWiden} vs ${pt.dealiasWiden} ($hAnnot1 vs $hAnnot2)")

                if (hAnnot1 && !hAnnot2) {
                  val tree1 = gen.mkMethodCall(marker_minibox2box.asInstanceOf[Symbol], List(oldTree.tpe.typeSymbol.tpeHK), List(oldTree))
                  adaptdbg(ind, "patching by inserting marker_minibox2box: " + tree1)
                  val tree2 = super.typed(tree1, mode, pt)
                  assert(tree2.tpe != ErrorType, tree2)
                  tree2

                } else if (!hAnnot1 && hAnnot2) {
                  assert(pt.typeSymbol.tpeHK != null, pt)
                  val tree1 = gen.mkMethodCall(marker_box2minibox.asInstanceOf[Symbol], List(oldTree.tpe.typeSymbol.tpeHK), List(oldTree))
//                  adaptdbg(ind, "patching by inserting marker_box2minibox: " + tree1)
//                  adaptdbg(ind, "old type: " + oldTpe)
//                  adaptdbg(ind, "old tree: " + oldTree)
                  val tree2 = super.typed(tree1, mode, pt)
                  assert(tree2.tpe != ErrorType, tree2)
                  tree2

                } else if ((hAnnot1 == hAnnot2) && (hAnnot1 == true)) {
                  // there's no need for adapting, but singleton types and annotations don't mix
                  // (check out mb_nested_class_fifth.scala for an example that crashes without
                  // this workaround)
                  tree.tpe = null
                  val tree0 = withNormalTyper(context) { _.typed(tree, mode, WildcardType) }
                  tree0.tpe = newTpe // just force the type and accept the -Ycheck complaints
                  tree0
                } else {
                  println()
                  adaptdbg(ind, "Don't know how to adapt tree:")
                  adaptdbg(ind, oldTree + " : " + oldTree.tpe)
                  adaptdbg(ind, s"to $pt")
                  adaptdbg(ind, "tree after typing: " + tree)
                  println()
                  adaptdbg(ind, "Error:")
                  adaptdbg(ind, err.toString)
                  adaptdbg(ind, "Types:")
                  adaptdbg(ind, s"  found:    $oldTpe (with underlying type ${oldTpe.dealiasWiden})")
                  adaptdbg(ind, s"  required: $newTpe (with underlying type ${newTpe.dealiasWiden})")
                  println("Can't adapt tree. Bailing out.")
                  sys.exit(1)
                }
//                if (hAnnot1 && !hAnnot2) {
//                  //println(marker_minibox2box.tpe)
//                  val ntree = gen.mkMethodCall(marker_minibox2box.asInstanceOf[Symbol], List(oldTree.tpe.typeSymbol.tpeHK), List(oldTree))
//                  withNormalTyper(context) { _.typed(ntree, mode, pt) }
//                } else if (!hAnnot1 && hAnnot2) {
//                  //println(marker_box2minibox.tpe)
//                  val ntree = super.typed(gen.mkMethodCall(marker_box2minibox.asInstanceOf[Symbol], List(oldTree.tpe.typeSymbol.tpeHK), List(oldTree)), mode, pt)
//                  withNormalTyper(context) { _.typed(ntree, mode, pt) }
              case SilentResultValue(res: Tree) =>
                assert(res.tpe != null)
                res
            }
            res
          case _ =>
            adaptdbg(ind, "[null tree]: " + tree)
            adaptdbg(ind, "[null type]: " + pt)
            // (new Exception()).printStackTrace()
            val tree2 = super.typed(tree, mode, pt)
            assert(tree2.tpe != null)
            tree2
        }
        adaptdbg(ind, " ==> " + res + ": " + res.tpe)
        if (res.tpe == ErrorType)
          adaptdbg(ind, "ERRORS: " + context.errBuffer)
        indent -= 1
        res
      }
    }
  }
}

