//
//     _____   .__         .__ ___.                    .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_ |__    ____  ___  ___|__|  ____     ____
//   /  \ /  \ |  | /    \ |  | | __ \  /  _ \ \  \/  /|  | /    \   / ___\
//  /    Y    \|  ||   |  \|  | | \_\ \(  <_> ) >    < |  ||   |  \ / /_/  >
//  \____|__  /|__||___|  /|__| |___  / \____/ /__/\_ \|__||___|  / \___  /
//          \/          \/          \/               \/         \/ /_____/
// Copyright (c) 2012-2014 Scala Team, École polytechnique fédérale de Lausanne
//
package miniboxing.plugin

import java.io.PrintWriter
import scala.tools.nsc.Global

trait MiniboxLogging {
  self: MiniboxInjectComponent =>

  val global: Global

  import global._

  def global_log(msg: => String) = if (settings.log.value.contains(phaseName)) global.log(msg)
  def log(msg: => Any) = if (flag_log) println(msg.toString) // TODO: Need to adapt tests to output miniboxing messages
  def mblog(msg: => Any) = log(msg)
  def debug(msg: => Any) = if (flag_debug) global_log(msg.toString)
  def stats(msg: => Any) = if (flag_stats) global_log(msg.toString)

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
