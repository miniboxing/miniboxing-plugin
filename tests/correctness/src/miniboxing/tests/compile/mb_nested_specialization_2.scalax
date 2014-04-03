package miniboxing.tests.compile.nested_rewiring.test2

class C[@miniboxed T, @miniboxed U]

class D[@miniboxed S] {
  def bar[@miniboxed T]() = {
    class F {
      class Z extends C[S, T]
    }
    ???
  }
}
