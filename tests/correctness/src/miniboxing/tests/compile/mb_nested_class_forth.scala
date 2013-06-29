package miniboxing.tests.compile.nested

import miniboxing.plugin.minispec

class CCCC[@minispec T](c: T) {
  class DDDD[@minispec U](d: U) {
    val x1: T = c
    val x2: U = d
    def foo = {
      val x3: T = c
      val x4: U = d
      println(x1 == x2)
      println(x3 == x4)
    }
  }
  def test = {
    new DDDD(c).foo
  }
}
