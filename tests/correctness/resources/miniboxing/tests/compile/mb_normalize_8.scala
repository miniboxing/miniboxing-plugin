package miniboxing.tests.correctness


class SpCls8Tgt[@miniboxed U] {
  def normalized[@miniboxed V](u: U, v: V): V = ???
}

class SpCls8[@miniboxed T] {
  def normalizeMe[@miniboxed S](sp: SpCls8Tgt[S], s: S, t: T): T = sp.normalized(s, t)
}

object SpCls8Static {
  def foo = new SpCls8Tgt[Int].normalized(3, 4)
  def bar[@miniboxed Z](z: Z) = new SpCls8Tgt[Int].normalized(3, z)
}
