package miniboxing.tests.correctness.erasure.torture1

/** Basic test for mb/erasure */
class A[@miniboxed T](val a: T, val b: T) {
  def foo(c: Boolean): Any = {
    // should not make a conversion:
    val x = if (c) a else b
    // will always make conversions, no matter the optimization:
    val y = a :: b :: Nil
    // should do a conversion:
    if (c) a else y
  }
}
