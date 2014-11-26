package miniboxing.tests.compile.bug150.warn4

object Test extends Object {

  def test[@miniboxed X](t: X) = {
    class D {
      // no warning here, foo is private so we can
      // transform its signature :)
      private def foo(): X = t
      override def toString(): String = "C with foo returning " + foo()
    };
    new D()
  }

  def main(args: Array[String]): Unit = {
    println(test(3).toString())
  }
}
