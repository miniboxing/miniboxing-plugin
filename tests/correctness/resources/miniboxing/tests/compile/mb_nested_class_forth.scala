package miniboxing.tests.compile.nested



class CCCC[@miniboxed T](c: T) {
  class DDDD[@miniboxed U](d: U) {
    val x1: T = c
    val x2: U = d
    def foo() = {
      val x3: T = c
      val x4: U = d
      println(x1 == x2)
      println(x3 == x4)
    }
  }
  def test() = {
    new DDDD(c).foo
  }
}
