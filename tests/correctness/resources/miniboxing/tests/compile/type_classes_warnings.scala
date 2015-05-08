class ImplicitNumeric[@miniboxed G](implicit n: Numeric[G]) {}
class ImplicitOrdering[@miniboxed G](implicit n: Ordering[G]) {}
class ImplicitOrdered[@miniboxed G](implicit n: Ordered[G]) {}
class ImplicitIntegral[@miniboxed G](implicit n: Integral[G]) {}
class ImplicitFractional[@miniboxed G](implicit n: Fractional[G]) {}

class ImplicitNumeric1[G](implicit n: Numeric[G]) {}
class ImplicitOrdering1[G](implicit n: Ordering[G]) {}
class ImplicitOrdered1[G](implicit n: Ordered[G]) {}
class ImplicitIntegral1[G](implicit n: Integral[G]) {}
class ImplicitFractional1[G](implicit n: Fractional[G]) {}

abstract class MyNumeric[@miniboxed Z] extends Numeric[Z] {}
abstract class MyOrdering[@miniboxed Z] extends Ordering[Z] {}
abstract class MyOrdered[@miniboxed Z] extends Ordered[Z] {}
abstract class MyIntegral[@miniboxed Z] extends Integral[Z] {}
abstract class MyFractional[@miniboxed Z] extends Fractional[Z] {}

object Test {

  def square1[@miniboxed T](a:T)(implicit n: Numeric[T]): T = {
    n.times(a, a)
  }

  def less1[@miniboxed T](a:T, b: T)(implicit o: Ordering[T]): Boolean = {
    o.lt(a, a)
  }

  def square2[@miniboxed T](a:T)(implicit n: Integral[T]): T = {
    n.times(a, a)
  }

  def square3[@miniboxed T](a:T)(implicit n: Fractional[T]): T = {
    n.times(a, a)
  }

  def main(args: Array[String]) : Unit = {
    square1(5)
    less1(1, 2)
    square2(5)
    square3(5.0)
    ()
  }
}
