package miniboxing.tests.correctness
import miniboxing.plugin.minispec

object SpCls7Obj {
  def normalized[@minispec U, @minispec V](u: U, v: V): U = ???
}

class SpCls7[@minispec S] {
  def normalizeMe[@minispec T](s: S, t: T): S = SpCls7Obj.normalized(s, t)
}
