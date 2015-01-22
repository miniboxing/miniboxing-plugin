package miniboxing.tests.compile.bug86

class C[@miniboxed T](t: T) {
  private def foo(t: Any): Unit = println("OK")
  def bar(t: T): Unit = foo(t)
}

object Test {
  def main(args: Array[String]): Unit = {
    (new C[String]("x")).bar("y")
    (new C[Double](1.0)).bar(2.0)
  }
}
