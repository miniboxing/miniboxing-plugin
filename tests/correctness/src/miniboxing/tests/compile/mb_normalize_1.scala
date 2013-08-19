package miniboxing.tests.correctness


object Obj {
  def normalizeMe1[@miniboxed T](t: T): T = t
}

class Cls {
  def normalizeMe2[@miniboxed T](t: T): T = t
}

class SpCls[@miniboxed S] {
  def normalizeMe3[@miniboxed T](t: T): T = t
}
