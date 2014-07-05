package miniboxing
package plugin
package transform
package interop
package commit

import scala.tools.nsc.transform.InfoTransform
import scala.tools.nsc.transform.TypingTransformers

trait InteropCommitTreeTransformer extends TypingTransformers {
  self: InteropCommitComponent =>

  import global._
  import definitions._

  def newTransformer(unit: CompilationUnit) =
    new InteropTreeTransformer(unit)

  class InteropTreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree) = tree
  }
}