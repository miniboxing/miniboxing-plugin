package miniboxing.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.InfoTransform
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.typechecker.Analyzer
import scala.reflect.internal.Names

trait ScalacCrossCompilingLayer {
  self =>

  val global: Global
  import global._

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

  object delambdafySupport {
    def isDelambdafyMethod(sym: Symbol): Boolean = sym.isMethod && sym.name.containsName(nme.ANON_FUN_NAME) && sym.isArtifact
    def isDelambdafyEnabled: Boolean = !global.settings.Ydelambdafy.isDefault
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

trait TweakedDuplicator extends Analyzer {
  import global._

  def tweakedEnsureCompanionObject(namer: Namer, cdef: ClassDef, creator: ClassDef => Tree): Option[Symbol]
  override def pluginsEnsureCompanionObject(namer: Namer, cdef: ClassDef, creator: ClassDef => Tree = companionModuleDef(_)): Symbol =
    tweakedEnsureCompanionObject(namer, cdef, creator) match {
      case Some(sym) => sym
      case None => super.pluginsEnsureCompanionObject(namer, cdef, creator)
    }
}

trait ScalacVersion {
  lazy val scalaBinaryVersion = "2.11"
  lazy val scalaVersion = "2.11.4"
}

