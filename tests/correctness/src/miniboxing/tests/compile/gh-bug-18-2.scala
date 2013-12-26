package miniboxing.tests.compile

import scala.annotation.tailrec

abstract class Dist2[@miniboxed A] {
  def apply(a: A): A

  def iterateUntil(): Dist2[A] = new Dist2[A] {
    def loop(a: A): A = a
    def apply(a: A): A = loop(a)
  }
}
