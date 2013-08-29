package miniboxing.plugin

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.TypingTransformers
import scala.collection.mutable.Set
import scala.tools.nsc.typechecker._
import scala.collection.mutable.{ Map => MMap }

trait MiniboxPostTreeTransformer extends TypingTransformers {
  self: MiniboxSpecComponent =>

  import global._
  import minibox._
  import definitions._
  import Flags._
  import typer.{ typed, atOwner }

  override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    override def transform(tree: Tree) = tree
  }
}
