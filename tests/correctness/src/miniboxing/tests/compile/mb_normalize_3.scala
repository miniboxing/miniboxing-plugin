package miniboxing.tests.correctness
import miniboxing.plugin.minispec

class SpCls3[@minispec S] {
  def normalizeMe[@minispec T](s: S, t: T): SpCls3[T] = new SpCls3[T]
}
