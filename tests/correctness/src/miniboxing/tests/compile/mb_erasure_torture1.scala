package miniboxing.tests.correctness.erasure.torture1

class A[@miniboxed T](val a: Array[T]) {
  def foo(c: Boolean): Any = {
    val x = if (c) a(0) else a(1)
    val y = a(0) :: a(1) :: Nil
    if (c) c else y
  }
}
