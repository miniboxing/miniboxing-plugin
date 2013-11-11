package miniboxing.tests.compile


object `package` {
  implicit val IntIntegral = new Integral[Long]
}

class Integral[T]
object LongRationals extends Rationals[Long]
abstract class Rationals[@miniboxed A](implicit integral: Integral[A])
