package miniboxing.tests.compile.bug162b

class A
class B[@miniboxed T](t: T) extends A {
  override def toString = "a"
}

object Test {
  def foo(a: A) = println("foo " + a)

  def main(args: Array[String]): Unit = {
    val b: B[Int] = new B(3)
    foo(b) // will fail :(
  }
}
