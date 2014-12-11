package miniboxing.tests.compile.bug166b

trait A[@miniboxed T] {
  def getStr: String = "aaa"
}
trait B[@miniboxed T] extends A[T] {
  override def getStr: String = super.getStr
  println(super.getStr)
  class X {
    def baz = println(B.super.getStr)
  }
}
class C extends B[Int] {
  override def toString = "instance of C"
}

object Test {
  def main(args: Array[String]): Unit =
    println(new C) /* force ClassLoader to try and load C */
}
