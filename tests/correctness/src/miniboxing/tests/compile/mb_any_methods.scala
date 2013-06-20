package miniboxing.tests.compile

import miniboxing.plugin.minispec

class CCC[@minispec T](val t: T) {
  def foo(t1: T, t2: Any) = {
    t.hashCode
    t == t1
    t == t2
    t.toString
  }
}
