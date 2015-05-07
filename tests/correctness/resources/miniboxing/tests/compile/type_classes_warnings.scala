class X[@miniboxed G](implicit n: Numeric[G]) {}

object Test {

  def square[@miniboxed T](a:T, x: Int)(implicit n: Numeric[T]): T = {
    n.times(a, a)
  }

  def main(args: Array[String]) : Unit = {
    square(5, 7)

    val s= new X[Int]()
    ()
  }
}

