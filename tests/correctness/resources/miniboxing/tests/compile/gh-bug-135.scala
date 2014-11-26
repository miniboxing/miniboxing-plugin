package miniboxing.tests.compile.bug135

object Test {
  def main(args: Array[String]): Unit = {
    val next: PartialFunction[Int, Int] = {
      case 0 => 1
      case 1 => 2
      case 2 => 3
    }
    println(next(0))
    println(next(1))
    println(next(2))
  }
}
