package miniboxing.tests.correctness
import miniboxing.plugin.minispec

case class SpCls4Tuple2[@minispec U, @minispec V](u: U, v: V)

class SpCls4[@minispec S] {
  def normalizeMe1[@minispec T](s: S, t: T) = new SpCls4Tuple2(s, t)
}
