package miniboxing.tests.correctness.erasure.torture1

class A[@miniboxed T](val a: T, val b: T) {
  def foo(c: Boolean): Any = {
    val x = if (c) a else b
    val y = a :: b :: Nil
    if (c) a else y
  }
}
