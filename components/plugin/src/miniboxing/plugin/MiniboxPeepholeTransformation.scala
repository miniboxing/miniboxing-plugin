package miniboxing.plugin

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.TypingTransformers
import scala.collection.mutable
import scala.tools.nsc.typechecker._

// TODO: Replace this, yaay!
trait MiniboxPeepholeTransformation extends TypingTransformers {
  self: MiniboxDuplComponent =>

  import global._
  import definitions._
  import Flags._
  import typer.{ typed, atOwner }

  /**
   * The tree transformer that simplifies constructs resulting from miniboxing
   */
  class MiniboxPeepholeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    import global._
    def afterMinibox[T](f: => T): T = atPhase(ownPhase.next)(f)
    override def transform(tree: Tree): Tree = peepholeTransform(tree)

    def peepholeTransform(tree: Tree): Tree = {
      curTree = tree
      // make sure specializations have been performed
      tree match {
        case t: SymTree => afterMinibox(t.symbol.info)
        case _ =>
      }

      tree match {

        // simplify minibox -> box -> minibox
        case Apply(TypeApply(b2m, List(tpt1)), List(Apply(TypeApply(m2b, List(tpt2)), List(inner, tag2)), tag1)) if b2m.symbol == box2minibox && m2b.symbol == minibox2box =>
          assert(tpt1.tpe.dealiasWiden =:= tpt2.tpe.dealiasWiden, "Peephole optimization checks: reduced types should be identical: " + showRaw(tpt1.tpe) + " vs " + showRaw(tpt2.tpe))
          assert(tag1.symbol == tag2.symbol, "Peephole optimization checks: reduced type tags should be identical: " + tag1 + " vs " + tag2 + " in " + currentMethod + " of " + currentClass + " in tree: \n" + tree)
          super.transform(inner)

        // simplify minibox -> box -> minibox
        case Apply(TypeApply(m2b, List(tpt1)), List(Apply(TypeApply(b2m, List(tpt2)), List(inner, tag2)), tag1)) if b2m.symbol == box2minibox && m2b.symbol == minibox2box =>
          assert(tpt1.tpe.dealiasWiden =:= tpt2.tpe.dealiasWiden, "Peephole optimization checks: reduced types should be identical: " + showRaw(tpt1.tpe) + " vs " + showRaw(tpt2.tpe))
          assert(tag1.symbol == tag2.symbol, "Peephole optimization checks: reduced type tags should be identical: " + tag1 + " vs " + tag2 + " in " + currentMethod + " of " + currentClass + " in tree: \n" + tree)
          super.transform(inner)

        // simplify equality between miniboxed values
        case Apply(Select(Apply(TypeApply(m2b1, List(tpt1)), List(val1, tag1)), eqeq), List(Apply(TypeApply(m2b2, List(tpt2)), List(val2, tag2)))) if m2b1.symbol == m2b2.symbol && m2b1.symbol == minibox2box && tree.symbol == Any_== =>
          val tree1 = (tag1, tag2) match {
            case (x, y) if (x == y) || (x.symbol == y.symbol) =>
              gen.mkMethodCall(notag_==, List(val1, val2))
            case _ =>
              gen.mkMethodCall(tag_==, List(val1, tag1, val2, tag2))
          }
          super.transform(localTyper.typed(tree1))

        // simplify equality between miniboxed values 2 - comparison with other values
        case Apply(Select(Apply(TypeApply(m2b1, List(tpt1)), List(val1, tag1)), eqeq), List(arg)) if m2b1.symbol == minibox2box && tree.symbol == Any_== =>
          val tree1 = gen.mkMethodCall(other_==, List(val1, tag1, arg))
          super.transform(localTyper.typed(tree1))

        // simplify hashCode
        case Apply(Select(Apply(TypeApply(m2b1, List(tpt1)), List(val1, tag1)), hash), _) if tree.symbol == Any_hashCode =>
          val tree1 = gen.mkMethodCall(tag_hashCode, List(val1, tag1))
          super.transform(localTyper.typed(tree1))

        // simplify toString
        case Apply(Select(Apply(TypeApply(m2b1, List(tpt1)), List(val1, tag1)), hash), _) if tree.symbol == Any_toString =>
          val tree1 = gen.mkMethodCall(tag_toString, List(val1, tag1))
          super.transform(localTyper.typed(tree1))

        // TODO: Transform:
        //  - no-effect minibox2box and box2minibox
        //  - known tag minibox2box and box2minibox
        //  - known tag and value minibox2box and box2minibox

        case _ =>
          super.transform(tree)
      }
    }
  }
}
