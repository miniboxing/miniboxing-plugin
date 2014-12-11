package miniboxing.tests.compile.field_messages

trait Z {
  def foo(): Int = 1
}

class C[@miniboxed T](c: Int) extends Z {
  override def foo(): Int = c
  println(c)
  private[this] val t: Int = c
  println(t)
  new C[Int](3)
  super.foo()

  def init() = {
    println(c)
    println(t)
    new C[Int](3)
    C.super.foo()
  }

  class D {
    println(c)
    println(t)
    new C[Int](3)
    C.super.foo()
  }
}
