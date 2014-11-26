package miniboxing.tests.compile.bug150.warn2

class C[@miniboxed X] {
  def foo(t: X): X = t
}

object Test {

  // there should be no warning, since C is
  // miniboxed and thus there will be a 
  // version foo_J that will be overridden
  def test[@miniboxed X](t: X) = {
    class D extends C[X] {
      override def foo(x: X): X = t
    }
    new D
  }

  def main(args: Array[String]): Unit = {
    println(test(3).foo(5))
  }
}
