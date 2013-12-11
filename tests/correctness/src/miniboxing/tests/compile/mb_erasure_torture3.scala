package miniboxing.tests.correctness.erasure.torture3

class A[@miniboxed T]() {
  def foo(c: T): Any = {
    val x: Any = c
    val hc = c.hashCode()
    val eq = c == 3
    val ts = c.toString
  }
}
