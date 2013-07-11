package miniboxing.tests.correctness
import miniboxing.plugin.minispec

class SpCls2[@minispec S] {
  def normalizeMe[@minispec T](s: S, t: T): T = t
}
