package miniboxing.tests.compile.bug102

class C[@specialized T] {
  def foo(t: T): T = null.asInstanceOf[T]
}

object Test extends App {
  println((new C[Int]).foo(3))
}
