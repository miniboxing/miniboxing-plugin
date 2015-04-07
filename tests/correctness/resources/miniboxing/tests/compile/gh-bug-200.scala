
object Test {
  def foo[@miniboxed T](t: T, dice: String): T =
    dice match {
      case "1" => 3.asInstanceOf[T]
      case "2" => t
    }

  def main(args: Array[String]): Unit = {
    println(foo(5, "1")) // should print 3
    println(foo(5, "2")) // should print 5
  }
}
