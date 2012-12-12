package miniboxing.plugin

import java.io.PrintWriter
import scala.tools.nsc.Global

trait MiniboxLogging {
  val global: Global
  
  def log(str: => String) = println(str)
  def debug(str: => String) = println(str)
  def logTree(label: String, tree: global.Tree) = {
    val showTrees = global.settings.Xshowtrees.value
    global.settings.Xshowtrees.value = true
    debug(label)
    debug("    " + global.showRaw(tree, true, true, false, false).replaceAll("\n", "\n    "))
    debug("\n\n")
    global.settings.Xshowtrees.value = showTrees
  }
  def logType(label: String, tpe: global.Type) = {
    debug(" *** TYPE LOGGING: " + label + " ***")
    debug("short: " + tpe)
    debug("long:\n" + global.typeDeconstruct.show(tpe))
    debug(" ******************" + "*" * label.length + "****")
  }
}