package miniboxing.tests.compile.bug159

object Test {
  class A[@miniboxed -T]
  class B[@miniboxed +T] extends A[T @annotation.unchecked.uncheckedVariance]
}
