package miniboxing.tests.correctness.erasure.torture6

/** This class checks that the return type 
 *  of a block can be changed without a double
 *  representation conversion */
class A[@miniboxed T <: Int]() {
  def foo(c: T): T = {
    println("hello!")
    c
  }
  // block should not be specialized as:
  // def foo_J(T_Tag: Byte, c: Long): Long = 
  //   box2minibox({ 
  //     println("hello!")
  //     minibox2box(c) 
  //   })
  // but as:
  // def foo_J(T_Tag: Byte, c: Long): Long = { 
  //   println("hello!")
  //   c 
  // }
}
