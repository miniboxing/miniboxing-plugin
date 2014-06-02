package miniboxing.tests.compile.bug76

class Hello[@miniboxed T] {
  case class Boo(x: Int)
  def foo(b: Boo): Unit = ???
}
