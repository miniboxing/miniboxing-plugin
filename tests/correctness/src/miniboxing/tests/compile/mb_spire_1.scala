package miniboxing.tests.compile
import miniboxing.plugin.minispec

object `package` {
  implicit val IntIntegral = new Integral[Long]
}

class Integral[T]
object LongRationals extends Rationals[Long]
abstract class Rationals[@minispec A](implicit integral: Integral[A])
