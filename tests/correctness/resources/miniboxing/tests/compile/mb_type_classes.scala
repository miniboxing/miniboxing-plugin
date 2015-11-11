package miniboxing.tests.compile.typeclasses

object Test {

  object TestNumeric {
    implicit object StringNumeric extends Numeric[String] {
      def plus(x: String, y: String): String = ???
      def minus(x: String, y: String): String = ???
      def times(x: String, y: String): String = ???
      def quot(x: String, y: String): String = ???
      def rem(x: String, y: String): String = ???
      def negate(x: String): String = ???
      def fromInt(x: Int): String = ???
      def toInt(x: String): Int = ???
      def toLong(x: String): Long = ???
      def toFloat(x: String): Float = ???
      def toDouble(x: String): Double = ???
      def compare(x: String, y: String): Int = ???
    }

    def foo[@miniboxed T: MiniboxedNumeric] =
      println(implicitly[MiniboxedNumeric[T]].getClass)
    def bar[T: Numeric] = foo[T]
  }

  object TestFractional {
    implicit object StringFractional extends Fractional[String] {
      def plus(x: String, y: String): String = ???
      def minus(x: String, y: String): String = ???
      def times(x: String, y: String): String = ???
      def quot(x: String, y: String): String = ???
      def rem(x: String, y: String): String = ???
      def negate(x: String): String = ???
      def fromInt(x: Int): String = ???
      def toInt(x: String): Int = ???
      def toLong(x: String): Long = ???
      def toFloat(x: String): Float = ???
      def toDouble(x: String): Double = ???
      def compare(x: String, y: String): Int = ???
      // Fractional
      def div(x: String, y: String): String = ???
    }

    def foo[@miniboxed T: MiniboxedFractional] =
      println(implicitly[MiniboxedFractional[T]].getClass)
    def bar[T: Fractional] = foo[T]
  }

  object TestIntegral {
    implicit object StringIntegral extends Integral[String] {
      def plus(x: String, y: String): String = ???
      def minus(x: String, y: String): String = ???
      def times(x: String, y: String): String = ???
      def quot(x: String, y: String): String = ???
      def rem(x: String, y: String): String = ???
      def negate(x: String): String = ???
      def fromInt(x: Int): String = ???
      def toInt(x: String): Int = ???
      def toLong(x: String): Long = ???
      def toFloat(x: String): Float = ???
      def toDouble(x: String): Double = ???
      def compare(x: String, y: String): Int = ???
      // Fractional
      def div(x: String, y: String): String = ???
    }

    def foo[@miniboxed T: MiniboxedIntegral] =
      println(implicitly[MiniboxedIntegral[T]].getClass)
    def bar[T: Integral] = foo[T]
  }

  object TestOrdering {
    def foo[@miniboxed T: MiniboxedOrdering] =
      println(implicitly[MiniboxedOrdering[T]].getClass)
    def bar[T: Ordering] = foo[T]
  }
  def main(args: Array[String]): Unit = {

    // Test Numeric:
    {
      import TestNumeric._
      println("MiniboxedNumeric direct:")
      foo[Char]
      foo[Byte]
      foo[Short]
      foo[Int]
      foo[Long]
      foo[Float]
      foo[Double]
      foo[String]
      println("MiniboxedNumeric through conversion:")
      bar[Char]
      bar[Byte]
      bar[Short]
      bar[Int]
      bar[Long]
      bar[Float]
      bar[Double]
      bar[String]
    }

    // Test Fractional:
    {
      import TestFractional._
      println("MiniboxedFractional direct:")
      foo[Float]
      foo[Double]
      foo[String]
      println("MiniboxedFractional through conversion:")
      bar[Float]
      bar[Double]
      bar[String]
    }

    // Test Integral:
    {
      import TestIntegral._
      println("MiniboxedIntegral direct:")
      foo[Char]
      foo[Byte]
      foo[Short]
      foo[Int]
      foo[Long]
      foo[String]
      println("MiniboxedIntegral through conversion:")
      bar[Char]
      bar[Byte]
      bar[Short]
      bar[Int]
      bar[Long]
      bar[String]
    }

    // Test Ordering:
    {
      import TestOrdering._
      println("MiniboxedOrdering direct:")
      foo[Char]
      foo[Byte]
      foo[Short]
      foo[Int]
      foo[Long]
      foo[Float]
      foo[Double]
      foo[String]
      println("MiniboxedOrdering through conversion:")
      bar[Char]
      bar[Byte]
      bar[Short]
      bar[Int]
      bar[Long]
      bar[Float]
      bar[Double]
      bar[String]
    }

  }
}
