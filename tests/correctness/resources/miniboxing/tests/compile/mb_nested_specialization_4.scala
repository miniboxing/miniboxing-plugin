package miniboxing.tests.compile.nested_rewiring.test4

class C[@miniboxed T, @miniboxed U]

class D[@miniboxed S] {
  def bar() = {
    class F {
      def foo[@miniboxed V](s: S, v: V) = {
        def zoo = {
          class Z extends C[S, V]
          new Z
        }
        zoo
      }
    }
    ???
  }
}
