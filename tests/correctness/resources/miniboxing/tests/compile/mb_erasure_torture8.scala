package miniboxing.tests.correctness.erasure.torture8

/** This class checks that the return type
 *  of a block can be changed without a double
 *  representation conversion */
class A[@miniboxed T <: Int]() {
  def foo(c: T, cond: Boolean): Any = {
    println("hello")
    c // expression in statement position meant to test transformation
    println("world")
    c
  }
  // the middle c should not be converted
  // to the boxed representation
}
