package miniboxing.tests.compile.bug117

object Test {
  class C[@specialized T, @miniboxed U](t: T, u: U)
  def foo[@specialized T, @miniboxed U](t: T, u: U) = ???
  def testNested = {
    class C[@specialized T, @miniboxed U](t: T, u: U)
    def foo[@specialized T, @miniboxed U](t: T, u: U) = ???
    ???
  }
}
