package miniboxing.tests.compile
import miniboxing.plugin.minispec

class BUG2[@minispec T](tail: BUG2[T]) {
  // WORKS:
  //def length: Int = tail.length
  // CRASHES:
  def length: Int = if (tail!=null) tail.length else 0
}
