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

      // 2.10 doesn't clear perRunCaches properly:
      minibox.metadata.warningPositions.clear()
      minibox.metadata.warningTypeParameters.clear()
    }
  }

  object CallNobridgeMethod {
    def unapply(tree: Tree): Boolean = tree match {
      case Apply(method, args) if method.symbol.hasAnnotation(interop.nobridgeClass) => true
      case _ => false
    }
  }


  // Check if there's another method with a similar signature in the owner,
  // otherwise it's not safe to remove this override. For more details, see
  // issue 228, where removing a bridge produces an AbstractMethodError.
  def duplicateSignatureExists(dd: Symbol): Boolean = {

    // shamelessly stolen from erasure:
    def sameTypeAfterErasure(sym1: Symbol, sym2: Symbol) =
      afterTweakErasure(sym1.info =:= sym2.info) && !sym1.isMacro && !sym2.isMacro

    val otherDecls = dd.owner.info.decls.filterNot(sym => sym == dd)
    val matchingDecls = otherDecls.filter(other => sameTypeAfterErasure(other, dd))
    !matchingDecls.isEmpty
  }

  class TweakErasureTransformer(unit: CompilationUnit) extends TreeRewriter(unit) {
    override def rewrite(tree: Tree) = tree match {
      case dd @ DefDef(mods, name, _, _, _, CallNobridgeMethod()) if dd.symbol.isBridge && duplicateSignatureExists(dd.symbol) =>
        dd.symbol.owner.info.members.unlink(dd.symbol)
//        println("wiped: " + dd)
        Nil
      case _ =>
        Descend
    }
  }
}
