package miniboxing.tests.compile.bug138

class Mb[@miniboxed T] {

  class Fun1[R] {
    def apply(t: T): R = ???
  }

  def foo(t: T) = { 

    // check nesting
    def bar = {
      class Fun2 extends Fun1[T] {
        override def apply(t: T): T = t
      }

      val f2 = new Fun2()
      val f1: Fun1[T] = f2
      println(f2.apply(t))
      println(f1.apply(t))
    }

    bar
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    new Mb[Int].foo(4)
    new Mb[String].foo("x")
    new Mb[Double].foo(3.0)
  }
}
