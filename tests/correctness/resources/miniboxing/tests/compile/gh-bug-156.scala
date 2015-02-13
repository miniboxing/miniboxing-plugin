package miniboxing.tests.compile.bug156

object Test {
  def main(args: Array[String]): Unit = {
    val f = (x: Int => Int) => x(3)
    val id = (z: Int) => z

    println(f(id))
  }
}
