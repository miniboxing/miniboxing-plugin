package miniboxing.tests.compile.bug38

// https://github.com/miniboxing/miniboxing-plugin/issues/38

class Vector[@miniboxed E](size: Int) {
  def this() = this(10)
}
