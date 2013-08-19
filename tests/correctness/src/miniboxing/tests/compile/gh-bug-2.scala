package miniboxing.tests.compile


class BUG2[@miniboxed T](tail: BUG2[T]) {
  // WORKS:
  //def length: Int = tail.length
  // CRASHES:
  def length: Int = if (tail!=null) tail.length else 0
}
