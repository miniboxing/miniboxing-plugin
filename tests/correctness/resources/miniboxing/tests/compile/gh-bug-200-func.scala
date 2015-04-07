
object Test {
  def foo[@miniboxed T](t: Int => Int, dice: String): Int => Int =
    dice match {
      case "1" => ((x: Int) => x + 1).asInstanceOf[Int => Int]
      case "2" => t
    }

  def main(args: Array[String]): Unit = {
    println(foo((x: Int) => x + 2, "1")(1)) // should print 2
    println(foo((x: Int) => x + 2, "2")(1)) // should print 3
  }
}
