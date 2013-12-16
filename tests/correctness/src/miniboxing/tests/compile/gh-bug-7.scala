package miniboxing.tests.compile


class BUG7[@miniboxed T](head: T, tail: BUG7[T]) {
  //override def hashCode(): Int = 123
  override def hashCode(): Int = tail.hashCode
}
