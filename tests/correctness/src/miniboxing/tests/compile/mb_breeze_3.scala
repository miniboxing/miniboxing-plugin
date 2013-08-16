package miniboxing.tests.compile.breeze3
import miniboxing.plugin.minispec

// Corresponds to bug #38 on the tracker
class Vector[@minispec E](size: Int) {
  def this() = this(10)
}
