package miniboxing.tests.compile.nested



class CCCCC[@miniboxed T](c: T) {
  def test = {
    class DDDDD[U](d: U) {
      val x1: T = c
      val x2: U = d
      def foo = {
        val x3: T = c
        val x4: U = d
        println(x1 == x2)
        println(x3 == x4)
      }
    }
    // pending on SI-7626: https://issues.scala-lang.org/browse/SI-7626?focusedCommentId=64763
    // new DDDDD(c).foo
  }
}
