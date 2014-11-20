package miniboxing.tests.compile.bug138

abstract class Fun[T, R] {
  def apply(t: T): R
}

object Test {

  def foo[@miniboxed T] = {
    def bar[@miniboxed R] = {
      class D extends Fun[T, R] {
        def apply(t: T): R = ???
      }
    }
  }
}
