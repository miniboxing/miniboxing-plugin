package miniboxning

import miniboxing.plugin.minispec

class Z[@minispec T](t: T) {
  def foo(t: T) = t
  def bar(t: T) = t
  val baz: T = ???
}
