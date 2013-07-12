package miniboxing.tests.correctness
import miniboxing.plugin.minispec

object SpCls6Obj {
  def normalized[@minispec U, @minispec V](u: U, v: V) = ???
}

class SpCls6[@minispec S] {
  def normalizeMe[@minispec T](s: S, t: T) = SpCls6Obj.normalized(s, t)
}
