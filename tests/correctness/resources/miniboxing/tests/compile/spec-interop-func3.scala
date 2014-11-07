package miniboxing.tests.compile.spec.interop.functions3

object Fuctions3 {
  class F[@miniboxed A, @miniboxed B] {
    def foo(b: B) = {
      val f0 = (x: A) => x
      val f1 = (x: A) => b
      val f2 = (x: A) => 3 / 0
      val f3 = (x: Int) => 4
      val f4 = (x: Int) => b
      val f5 = (x: A, y: B) => 3 / 0
      val f6 = (x: A, y: B) => y
    }
  }

  def foo() = {
    val f = (x: Int) => x
  }
}
