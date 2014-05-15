package miniboxing.tests.compile.bug90

trait Fun0[@miniboxed -T, @miniboxed +R] {
  def apply(r: T): R
}

object Test {
  def main(args: Array[String]): Unit = {
    val fun = 
      new Fun0[Double, Double] {
        def apply(r: Double): Double = {
          println(r.toString)
          r
        }
      }
    run(fun, 3.0)
  }

  def run[@miniboxed T](f: Fun0[T, Any], t: T): Any = {
    f(t)
  }
}
