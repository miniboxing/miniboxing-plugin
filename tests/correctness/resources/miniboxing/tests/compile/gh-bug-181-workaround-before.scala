package miniboxing.tests.bug181
import annotation.tailrec

// Thanks to @julien-truffaut for spotting this bug in his `brique`
// project: https://github.com/julien-truffaut/brique

class ConsList[@miniboxed A] {
  @tailrec final def dropWhile(p: A => Boolean): ConsList[A] = this match {
    case Cons(h,t) => if (p(h)) t.dropWhile(p) else this
    case CNil()    => this
  }
}

case class Cons[@miniboxed A](val head: A, val tail: ConsList[A]) extends ConsList[A]
case class CNil[@miniboxed A]() extends ConsList[A]
