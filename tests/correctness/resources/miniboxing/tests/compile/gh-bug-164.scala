package miniboxing.tests.compile.bug164

final class RRB[@miniboxed A] {
  protected def result: Int = 4
  println(result)
}

object Test {
  def main(args: Array[String]): Unit = {
    val v = new RRB[Int]
  }

}
