package miniboxing.tests.compile
import miniboxing.plugin.minispec

abstract class Dist1[@minispec A] {
  def apply(a: A): A

  def iterateUntil() = new Dist1[A] {
    def apply(a: A): A = ???
  }
}
