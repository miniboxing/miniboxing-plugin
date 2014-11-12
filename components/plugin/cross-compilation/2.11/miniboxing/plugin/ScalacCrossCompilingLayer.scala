package miniboxing.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.InfoTransform
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.typechecker.Analyzer

trait ScalacCrossCompilingLayer {
  self =>

  val global: Global

  implicit class RichGlobal(glb: global.type) {
    def beforePhase[T](phase: Phase)(f: => T): T =
      global.enteringPhase(phase)(f)
    def afterPhase[T](phase: Phase)(f: => T): T =
      global.exitingPhase(phase)(f)
  }

  type Mode = scala.reflect.internal.Mode

  def turnOffErrorReporting(analyzer: Analyzer)(context: analyzer.Context) = {
    // copy pasted from the impl
    import scala.tools.nsc.typechecker.ContextMode
    // context.set(disable = ContextMode.ReportErrors)
  }

  class TweakedAnalyzer extends scala.tools.nsc.typechecker.Analyzer {
    lazy val global: self.global.type = self.global
    import global._

    class TweakedTyper(context0: Context) extends Typer(context0) {
      override val infer = new Inferencer {
        def context = TweakedTyper.this.context
        // As explained in #132, the inferencer can refer to private
        // members and we don't want to crash in the retyper due to
        // this => we just replace the check. :)
        override def checkAccessible(tree: Tree, sym: Symbol, pre: Type, site: Tree): Tree =
          tree.setSymbol(sym).setType(pre.memberType(sym))
      }
    }
  }
}

trait ScalacVersion {
  lazy val scalaBinaryVersion = "2.11"
  lazy val scalaVersion = "2.11.4"
}

