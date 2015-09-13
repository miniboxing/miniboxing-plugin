package miniboxing.compile.tests.bug240.full

object Test {
  val fs = new EndofunctionSpace[Double] {}
  val f = fs.composeN((x: Double) => x * x, 4)
  val f2 = f(2)
}
