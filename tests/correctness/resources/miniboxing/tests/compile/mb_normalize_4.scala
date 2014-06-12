package miniboxing.tests.correctness


case class SpCls4Tuple2[@miniboxed U, @miniboxed V](u: U, v: V)

class SpCls4[@miniboxed S] {
  def normalizeMe1[@miniboxed T](s: S, t: T) = new SpCls4Tuple2(s, t)
}
