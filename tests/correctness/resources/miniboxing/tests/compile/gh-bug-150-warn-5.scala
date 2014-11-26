package miniboxing.tests.compile.bug150.warn5

class C[X] {
  def foo(t: X): X = t
}

object Test {

  def test[@miniboxed X](t: X) =
    new C[X] {
      // this time, the method overrides a generic
      // method, so the complaint should be that
      // class C could benefit from miniboxing
      // type parameter X
      override def foo(x: X): X = t
    }

  def main(args: Array[String]): Unit = {
    println(test(3).foo(5))
  }
}
