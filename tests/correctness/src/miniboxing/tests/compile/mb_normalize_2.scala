package miniboxing.tests.correctness


class SpCls2[@miniboxed S] {
  def normalizeMe[@miniboxed T](s: S, t: T): T = t
}
