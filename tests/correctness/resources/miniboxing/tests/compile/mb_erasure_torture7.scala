package miniboxing.tests.correctness.erasure.torture7

/** This class checks that the return type 
 *  of a block can be changed without a double
 *  representation conversion */
class A[@miniboxed T <: Int]() {
  def foo(c: T, cond: Boolean): T = {
    println("hello")
    if (cond) { println("world"); c } else ???
  }
  // there should not be any other representation
  // conversion other than for "???"
}
