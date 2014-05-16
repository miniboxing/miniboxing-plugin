package miniboxing.tests.compile.bug993.partial

class C[@miniboxed T] {
  def foo = 3
  def bar(t: T): T = t
}
