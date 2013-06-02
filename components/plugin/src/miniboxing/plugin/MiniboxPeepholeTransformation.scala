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

//        case Apply(Select(Apply(TypeApply(b2m, List(tpt1)), List(val1, tag1)), Any_==), args) if b2m.symbol == box2minibox =>
//          println("XXX")
//          super.transform(tree)

        case _ =>
          super.transform(tree)
      }
    }
  }
}
