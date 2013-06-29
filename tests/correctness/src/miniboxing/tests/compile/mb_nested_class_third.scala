package miniboxing.tests.compile.nested

import miniboxing.plugin.minispec

class CCC[@minispec T](c: T) {
  class DDD[@minispec U](d: U) {
    def foo = println(c == d)
  }
  def test = {
    new DDD(c).foo
  }
}
