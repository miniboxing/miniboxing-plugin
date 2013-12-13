package miniboxing.tests.correctness.erasure.torture4

class A[@miniboxed T <: Int]() {
  def foo(c: T): Any = {
    val x: Int = c // c should get converted to the boxed representation
    val y: Int = c + x
    val z: Int = x + c
    println(x)
    println(y)
    println(z)
    println(c)
  }
}
