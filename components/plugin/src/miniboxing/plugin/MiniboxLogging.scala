package miniboxing.plugin

import java.io.PrintWriter
import scala.tools.nsc.Global

trait MiniboxLogging {
  self: MiniboxDuplComponent =>

  val global: Global

  def log(msg: => Any) = if (flag_log) println(msg.toString)
  def mblog(msg: => Any) = log(msg)
  def debug(msg: => Any) = if (flag_debug) println(msg.toString)
  def stats(msg: => Any) = if (flag_stats) println(msg.toString)

  def printTree(label: Any, tree: global.Tree) = {
//    val showTrees = global.settings.Xshowtrees.value
//    global.settings.Xshowtrees.value = true
    debug(" *** TREE LOGGING: " + label + " ***")
    debug("short: " + tree)
    debug("long:\n" + global.showRaw(tree, true, true, false, false).replaceAll("\n", "\n    "))
    debug(" ******************" + "*" * label.toString.length + "****")
//    global.settings.Xshowtrees.value = showTrees
  }
  def printType(label: Any, tpe: global.Type) = {
    debug(" *** TYPE LOGGING: " + label + " ***")
    debug("short: " + tpe)
    debug("long:\n" + global.typeDeconstruct.show(tpe))
    debug(" ******************" + "*" * label.toString.length + "****")
  }
}
