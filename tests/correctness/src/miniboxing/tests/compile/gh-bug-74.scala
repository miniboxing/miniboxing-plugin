package miniboxing.tests.compile.bug74

trait Hello[@miniboxed T] {
  class Boo(x: Int)
  def foo(t: T, b: Boo): Unit = ???
}
