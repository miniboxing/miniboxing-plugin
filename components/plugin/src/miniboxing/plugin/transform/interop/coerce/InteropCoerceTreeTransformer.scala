package miniboxing
package plugin
package transform
package interop
package coerce

import scala.tools.nsc.transform.InfoTransform
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.transform.InfoTransform

trait InteropCoerceTreeTransformer extends InfoTransform with TypingTransformers {
  self: InteropCoerceComponent =>

  import global._
  import definitions._

  override def newTransformer(unit: CompilationUnit) = new TypingTransformer(unit) {
    def apply(tree: Tree) = tree
  }

  override def transformInfo(sym: Symbol, tpe: Type): Type =
    tpe

  class CoercePhase(prev: Phase) extends StdPhase(prev) {
    override def name = InteropCoerceTreeTransformer.this.phaseName
    override def checkable = false
    def apply(unit: CompilationUnit): Unit = {
      ()
    }
  }
}