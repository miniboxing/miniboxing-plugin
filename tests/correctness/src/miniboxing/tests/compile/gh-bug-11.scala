package miniboxing.tests.compile


class Order[T]

class X[@miniboxed X] {
  def foo[T](o: Order[T]) = o
}
