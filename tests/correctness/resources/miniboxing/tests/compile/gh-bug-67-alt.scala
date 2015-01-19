package miniboxing.tests.compile.bug67

trait C[@miniboxed T]
class D extends C[Int]
class E[@miniboxed T] extends C[T]

object Test {
  def main(args: Array[String]): Unit = {
    new D
    new E
    println("OK")
  }
}
