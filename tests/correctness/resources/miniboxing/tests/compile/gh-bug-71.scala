package miniboxing.tests.compile.bug71

class Test[@miniboxed T] {
  def foo(t: T): T = Test.id(t)
}

object Test {
  private final def id[T](t: T): T = t
}
