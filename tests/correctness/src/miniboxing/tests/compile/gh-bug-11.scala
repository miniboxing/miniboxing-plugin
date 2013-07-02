package miniboxing.tests.compile
import miniboxing.plugin.minispec

class Order[T]

class X[@minispec X] {
  def foo[T](o: Order[T]) = o
}
