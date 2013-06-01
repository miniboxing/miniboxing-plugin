package miniboxing.plugin

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.TypingTransformers
import scala.collection.mutable
import scala.tools.nsc.typechecker._

trait MiniboxPeepholeTransformation extends TypingTransformers {
  self: MiniboxComponent =>

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
          assert(tpt1.tpe =:= tpt2.tpe, "Peephole optimization checks: reduced types should be identical.")
          assert(tag1.symbol == tag2.symbol, "Peephole optimization checks: reduced type tags should be identical.")
          super.transform(inner)

//        /*
//         * Array creation in miniboxed code is written by the user as:
//         *   Manifest[T].newArray[T](len)
//         * and we rewrite it to:
//         *   MiniboxArray.internal_newArray(len, tagOfT)
//         */
//        case Apply(TypeApply(meth, tpe :: Nil), len :: Nil) if (tree.symbol == newArray) =>
//          localTyper.typedPos(tree.pos)(
//            gen.mkMethodCall(internal_newArray, List(transform(len), getTag(tpe))))
//
//        // array_length with tag-based dispatch
//        case Select(qual, meth) if isMiniboxedArray(qual) && tree.symbol == Array_length =>
//          localTyper.typedPos(tree.pos)(
//            gen.mkMethodCall(array_length, List(transform(qual))))
//
        case _ =>
          super.transform(tree)
      }
    }
  }
}
