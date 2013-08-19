package miniboxing.tests.compile

import scala.annotation.tailrec

abstract class DistZZZ[@miniboxed A] {
  def apply(a: A): A

  def iterateUntil(): DistZZZ[A] = new DistZZZ[A] {
    def loop(a: A): A = a
    def apply(a: A): A = loop(a)
  }
}
