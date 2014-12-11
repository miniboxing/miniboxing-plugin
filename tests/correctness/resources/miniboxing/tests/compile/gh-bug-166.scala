package miniboxing.tests.compile.bug166

trait A[@miniboxed T] {
  def getStr: String = "aaa"
}
trait B[@miniboxed T] extends A[T] {
  override def getStr: String = super.getStr
  override def toString = "instance of C"
}
class C extends B[Int]

object Test {
  def main(args: Array[String]): Unit =
    println(new C) /* force ClassLoader to try and load C */
}
