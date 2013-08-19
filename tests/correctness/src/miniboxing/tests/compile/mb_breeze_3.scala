package miniboxing.tests.compile.breeze3


// Corresponds to bug #38 on the tracker
class Vector[@miniboxed E](size: Int) {
  def this() = this(10)
}
