package miniboxing.plugin
package transform
package tweakerasure

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.TypingTransformers
import scala.reflect.internal.Phase
import miniboxing.plugin.transform.infrastructure.TreeRewriters

trait TweakErasureTreeTransformer extends TreeRewriters with ScalacCrossCompilingLayer {
  self: TweakErasureComponent =>

  import global._

  class TweakErasurePhase(prev: Phase) extends StdPhase(prev) {
    override def name = TweakErasureTreeTransformer.this.phaseName
    def apply(unit: CompilationUnit): Unit = {
      val transformer = new TweakErasureTransformer(unit)
      transformer.transformUnit(unit)
    }
  }

  object CallNobridgeMethod {
    def unapply(tree: Tree): Boolean = tree match {
      case Apply(method, args) if method.symbol.hasAnnotation(interop.nobridgeClass) => true
      case _ => false
    }
  }

  class TweakErasureTransformer(unit: CompilationUnit) extends TreeRewriter(unit) {
    override def rewrite(tree: Tree) = tree match {
      case dd @ DefDef(mods, name, _, _, _, CallNobridgeMethod()) if dd.symbol.isBridge =>
//        println("wiped: " + dd)
        Nil
      case _ =>
        Descend
    }
  }
}
