package miniboxing.tests.compile.nested

import miniboxing.plugin.minispec

class CC[@minispec T](c: T) {
  class DD[@minispec U](d: U)
  def test = {
    new DD(c)
  }
}
