package miniboxing.tests.compile


abstract class Dist1[@miniboxed A] {
  def apply(a: A): A

  def iterateUntil() = new Dist1[A] {
    def apply(a: A): A = ???
  }
}
