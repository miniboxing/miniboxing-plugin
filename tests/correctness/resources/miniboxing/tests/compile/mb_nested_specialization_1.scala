package miniboxing.tests.compile.nested_rewiring.test1

class C[@miniboxed T, @miniboxed U]

class D[@miniboxed S] {
  def bar() = {
    class F[@miniboxed T] {
      def foo(t: T) = {
        class Z extends C[S, T]
        new Z
      }
    }
    ???
  }
}
