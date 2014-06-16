package miniboxing.tests.compile.bug107

import annotation.tailrec

object Test {
  def foldDigitsLeft[@miniboxed A](a: A)(f: (A, Int) => A): A = {
    @tailrec def recur(next: Natural, sofar: A): A = next match {
      case End(d) => f(a, d)
      case Digit(d, tail) => recur(tail, f(a, d))
    }
    recur(???, a)
  }

  abstract class Natural
  case class End(d: Int) extends Natural
  case class Digit(d: Int, tl: Natural) extends Natural
}
