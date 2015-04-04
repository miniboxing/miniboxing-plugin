package miniboxing.tests.bug182
import annotation.tailrec

sealed abstract class ConsList[@miniboxed A] {
  import ConsList._

  def dropWhile(): ConsList[A] = this match {
    case Cons(h, t) => t
  }
}

object ConsList {
  class Cons[@miniboxed A](val head: A, val tail: ConsList[A]) extends ConsList[A]
  object Cons{ 
    def unapply[@miniboxed A](x: Cons[A]): Option[(A, ConsList[A])] = 
      if (x == null)
        None
      else
        Some((x.head, x.tail))
  }
}
