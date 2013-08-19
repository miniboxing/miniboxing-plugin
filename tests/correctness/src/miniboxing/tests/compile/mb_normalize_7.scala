package miniboxing.tests.correctness


object SpCls7Obj {
  def normalized[@miniboxed U, @miniboxed V](u: U, v: V): U = ???
}

class SpCls7[@miniboxed S] {
  def normalizeMe[@miniboxed T](s: S, t: T): S = SpCls7Obj.normalized(s, t)
}
