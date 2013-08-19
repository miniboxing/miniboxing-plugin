package miniboxing.tests.compile


class BUG8[@miniboxed T](tail: BUG8[T]) {
  def contains(e: T): Boolean = tail.contains(e)
}
