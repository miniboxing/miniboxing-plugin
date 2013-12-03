package miniboxing.plugin

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.TypingTransformers
import scala.collection.mutable.Set
import scala.tools.nsc.typechecker._
import scala.collection.mutable.{ Map => MMap }

trait MiniboxAdaptTreeTransformer extends TypingTransformers {
  self: MiniboxAdaptComponent =>

  import global._
//  import definitions._
//  import minibox._
//  import Flags._
//  import typer.{ typed, atOwner }

  def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    override def transform(tree: Tree): Tree = tree
  }
}
