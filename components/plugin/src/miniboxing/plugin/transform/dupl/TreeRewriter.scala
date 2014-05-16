package miniboxing.plugin.dupl

import miniboxing.plugin.MiniboxDuplComponent
import scala.language.implicitConversions

trait TreeRewriters {
  this: MiniboxDuplComponent =>

  import global._
  def log(msg: => String) = if (settings.log.value.contains(phaseName)) log(msg)

  abstract class TreeRewriter(unit: CompilationUnit) extends TypingTransformer(unit) {
    sealed trait Result
    case class Single(tree: Tree) extends Result { override def toString = tree.toString }
    case class Multi(trees: List[Tree]) extends Result { override def toString = trees.toString }
    implicit def treeToResult(tree: Tree): Result = Single(tree)
    implicit def treesToResult(trees: List[Tree]): Result = Multi(trees)

    def typed(tree: Tree) = {
      try localTyper.typed(tree)
      catch { case err: TypeError => println(tree); println(showRaw(tree, printIds = true, printTypes = true)); throw err }
    }

    override def transform(tree: Tree): Tree = {
      val treeString = if (settings.log.value.contains(phaseName)) atPhase(globalPhase)(tree.toString) else ""
      val treeTpeString = if (settings.log.value.contains(phaseName)) s"${tree.tpe}" else ""
      def commit(rule: Option[String], tree1: Result): Result = {
        val rulePrefix = rule.map(rule => s"$rule) ").getOrElse("")
        def logRewrite() = log(s"$rulePrefix$treeString -> $tree1")
        def logRetype() = log(s"$rulePrefix$treeString: $treeTpeString -> ${tree1.asInstanceOf[Single].tree.tpe}")
        val tree2 = tree1 match {
          case Single(tree1) => if (tree ne tree1) logRewrite(); if ((tree eq tree1) && (tree.tpe ne tree1.tpe)) logRetype(); typed(tree1)
          case Multi(trees1) => logRewrite(); typed(Block(trees1: _*))
        }
        transform(tree2)
      }
      def fallback(): Result = super.transform(tree)
      rewrite(tree)(State(tree, currentOwner)).lift(tree).getOrElse(fallback()) match {
        case Single(tree1) => tree1
        case Multi(trees1) => Block(trees1: _*)
      }
    }

    def typedStats(stats: List[Tree], exprOwner: Symbol) = {
      try localTyper.typedStats(stats, exprOwner)
      catch { case err: TypeError => println(stats); println(showRaw(stats, printIds = true, printTypes = true)); throw err }
    }

    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      stats flatMap {
        case stat =>
          val statString = if (settings.log.value.contains(phaseName)) atPhase(globalPhase)(stat.toString) else ""
          val statTpeString = if (settings.log.value.contains(phaseName)) s"${stat.tpe}" else ""
          def commit(rule: Option[String], stats1: Result): Result = {
            val rulePrefix = rule.map(rule => s"$rule) ").getOrElse("")
            def logRewrite() = log(s"$rulePrefix$statString -> $stats1")
            def logRetype() = log(s"$rulePrefix$statString: $statTpeString -> ${stats1.asInstanceOf[Single].tree.tpe}")
            val stats2 = stats1 match {
              case Single(stat1) => if (stat ne stat1) logRewrite(); if ((stat eq stat1) && (stat.tpe ne stat1.tpe)) logRetype(); List(typed(stat1))
              case Multi(stats1) => logRewrite(); typedStats(stats1, exprOwner)
            }
            transformStats(stats2, exprOwner)
          }
          def fallback(): Result = {
            if (exprOwner != currentOwner && stat.isTerm) atOwner(exprOwner)(super.transform(stat))
            else super.transform(stat)
          }
          rewrite(stat)(State(stat, if (stat.isTerm) exprOwner else currentOwner)).lift(stat).getOrElse(fallback()) match {
            case Single(stat1) => List(stat1)
            case Multi(stats1) => stats1
          }
      }
    }

    case class State(tree: Tree, owner: Symbol)

    // Core
    def rewrite(tree: Tree)(implicit state: State): PartialFunction[Tree, Result]
  }
}
