package miniboxing.tests.correctness


object SpCls6Obj {
  def normalized[@miniboxed U, @miniboxed V](u: U, v: V) = ???
}

class SpCls6[@miniboxed S] {
  def normalizeMe[@miniboxed T](s: S, t: T) = SpCls6Obj.normalized(s, t)
}
