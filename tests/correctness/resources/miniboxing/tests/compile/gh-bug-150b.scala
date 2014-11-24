package miniboxing.tests.compile.spec.interop.functions3

object Fuctions3 {
  class F[@miniboxed A] {
    def foo[B](b: B) = {
      val f6 = (x: A, y: B) => y
    }
  }
}
