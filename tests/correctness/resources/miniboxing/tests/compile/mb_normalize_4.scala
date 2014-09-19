package miniboxing.tests.correctness


class SpCls4Tuple2[@miniboxed U, @miniboxed V](val u: U, val v: V)

class SpCls4[@miniboxed S] {
  def normalizeMe1[@miniboxed T](s: S, t: T) = new SpCls4Tuple2(s, t)
}
