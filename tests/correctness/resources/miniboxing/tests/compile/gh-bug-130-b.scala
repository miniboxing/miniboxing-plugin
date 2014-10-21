package miniboxing.tests.compile.bug130b

object Test {
  class C[@miniboxed T]
  def boo[@miniboxed X] = new C[X]
  def baz1[X] = new C[X]
  def baz2[X] = boo[X]
  class D1[X] { new C[X] }
  class D2[X] { boo[X] }
}
