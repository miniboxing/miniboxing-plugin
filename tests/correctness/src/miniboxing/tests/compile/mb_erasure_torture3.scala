package miniboxing.tests.correctness.erasure.torture3

/** Conversion test for mb/erasure */
class A[@miniboxed T]() {
  def foo(c: T): Any = {
    // should convert to boxed:
    val x: Any = c
    // should rewire {hashcode,==,toString} to mb_runtime*
    val hc = c.hashCode()
    val eq = c == 3
    val ts = c.toString
  }
}
