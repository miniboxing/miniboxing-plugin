package miniboxing.tests.compile.bug150.warn1

class C[X] {
  def foo(t: X): X = t
}

object Test {

  // there should be a warning, but since C may be
  // miniboxed, we shouldn't warn when foo is not
  // transformed, but when transforming D itself.
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
