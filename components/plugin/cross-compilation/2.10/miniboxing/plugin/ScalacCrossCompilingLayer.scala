package miniboxing.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.InfoTransform
import scala.tools.nsc.transform.TypingTransformers
import scala.reflect.ClassTag
import scala.tools.nsc.typechecker.Analyzer

trait ScalacCrossCompilingLayer {

  val global: Global

  import global._

  type Mode = Int

  implicit class CompatTree(tree: Tree) {
    def hasSymbolField: Boolean = tree.hasSymbol
    def hasAttachment[T: ClassTag]: Boolean = tree.attachments.get[T].isDefined
  }

  implicit class CompatTermName(name: TermName) {
    import nme._

    def dropLocal: TermName            = name.toTermName stripSuffix encode(LOCAL_SUFFIX_STRING)
    def dropSetter: TermName           = name.toTermName stripSuffix SETTER_SUFFIX
    def localName: TermName            = getterName append encode(LOCAL_SUFFIX_STRING)
    def setterName: TermName           = getterName append SETTER_SUFFIX
    def getterName: TermName           = dropSetter.dropLocal
  }

  def turnOffErrorReporting(analyzer: Analyzer)(context: analyzer.Context) = {
    // copy pasted from the impl
    val ReportErrors     = 1 << 0
    context.restoreState(context.state & ~ReportErrors)
  }

  lazy val noSelfType = emptyValDef
}

trait ScalacVersion {
  lazy val scalaBinaryVersion = "2.10"
  lazy val scalaVersion = "2.10.4"
}

