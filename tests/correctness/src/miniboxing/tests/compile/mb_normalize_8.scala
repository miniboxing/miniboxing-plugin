package miniboxing.tests.correctness
import miniboxing.plugin.minispec

class SpCls8Tgt[@minispec U] {
  def normalized[@minispec V](u: U, v: V): V = ???
}

class SpCls8[@minispec T] {
  def normalizeMe[@minispec S](sp: SpCls8Tgt[S], s: S, t: T): T = sp.normalized(s, t)
}

object SpCls8Static {
  def foo = new SpCls8Tgt[Byte].normalized(3, 4)
  def bar[@minispec Z](z: Z) = new SpCls8Tgt[Int].normalized(3, z)
}
