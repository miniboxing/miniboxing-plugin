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
  self =>

  val global: Global

  import global._

  type Mode = Int

  implicit class CompatSym(sym: Symbol) {
    import scala.reflect.internal.Flags._
    def isPrivateThis = (sym hasFlag PRIVATE) && (sym hasFlag LOCAL)
  }

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

  implicit class CompatScope(scope: Scope) {
    def enterIfNew[T <: Symbol](sym: T): T = {
      val existing = scope.lookupEntry(sym.name)
      if (existing == null) scope.enter(sym)
      else existing.sym.asInstanceOf[T]
    }

  }

  def turnOffErrorReporting(analyzer: Analyzer)(context: analyzer.Context) = {
    // copy pasted from the impl
    val ReportErrors     = 1 << 0
    context.restoreState(context.state & ~ReportErrors)
  }

  lazy val noSelfType = emptyValDef

  class TweakedAnalyzer extends scala.tools.nsc.typechecker.Analyzer {
    lazy val global: self.global.type = self.global
    import global._

    class TweakedTyper(context0: Context) extends Typer(context0) {
      override val infer = new Inferencer(context0) {
        // As explained in #132, the inferencer can refer to private
        // members and we don't want to crash in the retyper due to
        // this => we just replace the check. :)
        override def checkAccessible(tree: Tree, sym: Symbol, pre: Type, site: Tree): Tree =
          tree.setSymbol(sym).setType(pre.memberType(sym))
      }
    }
  }

  object delambdafySupport {
    def isDelambdafyMethod(sym: Symbol): Boolean = false
    def isDelambdafyEnabled: Boolean = false
  }

  def newDefDef(sym: Symbol, rhs: Tree)(): DefDef = (
    DefDef(sym, vparamss => rhs.substituteSymbols(sym.paramss.flatten, vparamss.flatten))
  )
}

trait TweakedDuplicator extends Analyzer {

  import global._

  def tweakedEnsureCompanionObject(namer: Namer, cdef: ClassDef, creator: ClassDef => Tree): Option[Symbol]
// we don't have this hook => bug #184 will be triggered
//  override def pluginsEnsureCompanionObject(namer: Namer, cdef: ClassDef, creator: ClassDef => Tree = companionModuleDef(_)): Symbol =
//    tweakedEnsureCompanionObject(namer, cdef, creator) match {
//      case Some(sym) => sym
//      case None => super.pluginsEnsureCompanionObject(namer, cdef, creator)
//    }
}

trait ScalacVersion {
  lazy val scalaBinaryVersion = "2.10"
  lazy val scalaVersion = "2.10.4"
}

