package miniboxing.tests.compile.bug104

class D[S] {
  def bar(s: S) = {
    class Z
    def foo = new Z
    ()
  }
}
