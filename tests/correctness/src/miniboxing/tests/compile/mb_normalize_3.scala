package miniboxing.tests.correctness


class SpCls3[@miniboxed S] {
  def normalizeMe[@miniboxed T](s: S, t: T): SpCls3[T] = new SpCls3[T]
}
