package miniboxing.tests.compile.bug94


class D[@miniboxed S] {
  def bar(s: S) = {
    class Z
    def foo = new Z
    ???
  }
}
