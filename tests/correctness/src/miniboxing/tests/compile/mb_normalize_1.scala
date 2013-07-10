package miniboxing.tests.correctness
import miniboxing.plugin.minispec

object Obj {
  def normalizeMe1[@minispec T](t: T): T = t
}

class Cls {
  def normalizeMe2[@minispec T](t: T): T = t
}

class SpCls[@minispec S] {
  def normalizeMe3[@minispec T](t: T): T = t
}
