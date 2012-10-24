package miniboxing.bugs
import miniboxing.plugin.minispec

class C[@minispec T](tail: C[T]) {
  // WORKS:
  //def length: Int = tail.length
  // CRASHES:
  def length: Int = if (tail!=null) tail.length else 0
}
