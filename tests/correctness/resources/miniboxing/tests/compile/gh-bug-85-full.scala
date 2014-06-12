package miniboxing.tests.compile.bug85.full

trait F1[@miniboxed A, @miniboxed B] { self =>
  def apply(a: A): B

  def andThen[@miniboxed C](f: F1[B, C]): F1[A, C] =
    new F1[A, C] { def apply(a: A): C = f(self(a)) }

  def compose[@miniboxed Z](f: F1[Z, A]): F1[Z, B] =
    new F1[Z, B] { def apply(z: Z): B = self(f(z)) }
}

object Main {
  def main(args: Array[String]): Unit = {
    val f1 = new F1[Int, Double] { def apply(x: Int): Double = (x + 1).toDouble }
    val f2 = new F1[Double, String] { def apply(n: Double): String = (n / 2).toString }
    val f3: F1[Int, String] = f1 andThen f2
    println(f3(10))
  }
}
