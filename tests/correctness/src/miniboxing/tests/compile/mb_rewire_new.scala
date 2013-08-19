package miniboxing.tests.compile



/**
 * We can distinguish 5 cases:
 * (1) creating an object from the current object, thus inheriting its specs
 * (2) creating an object from outside, with type params:
 *     2.1. fixed, specializable (Int, Long)
 *     2.2. fixed, not specializable (String)
 *     2.3. from the current scope, but not specialized
 *     2.4. from the current scope, but specialized => same as (1)
 */
class TN[@miniboxed T](val t: T){
  def foo(): TN[T] =
    new TN[T](t)           // case (1)
}

object TestNewRewire {
  def test[T](t: T) = {
    new TN[Int](1)         // case (2.1)
    new TN[String]("xxx")  // case (2.2)
    new TN[T](t)           // case (2.3)
    new String("yyy")      // should not interfere
  }
}
