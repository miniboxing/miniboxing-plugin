package miniboxing.tests.correctness.erasure.torture2

/** Array test for mb/erasure */
class A[@miniboxed T](val a: Array[T]) {
  def foo(c: Boolean): Any = {
    // should not make conversions, instead should rewire to mb_array*
    val x = if (c) a(0) else a(1)
    // should use ScalaRunTime to access arrays, as we don't need boxing
    val y = a(0) :: a(1) :: Nil
    // should make a conversion:
    if (c) x else y
  }
}
