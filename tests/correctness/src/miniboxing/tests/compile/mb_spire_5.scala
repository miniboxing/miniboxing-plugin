package miniboxing.tests.compile
import miniboxing.plugin.minispec
import scala.annotation.tailrec

abstract class DistZZZ[@minispec A] {
  def apply(a: A): A

  def iterateUntil(): DistZZZ[A] = new DistZZZ[A] {
    def loop(a: A): A = a
    def apply(a: A): A = loop(a)
  }
}
