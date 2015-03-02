package miniboxing.tests.compile.off


class C[@miniboxed T](t: T) {
  def f: T => T = (t: T) => t
  def bar = f(t)
}

object Test extends C[Int](3)
