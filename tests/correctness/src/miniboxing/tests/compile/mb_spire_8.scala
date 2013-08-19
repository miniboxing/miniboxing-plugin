package miniboxing.tests.compile

import scala.annotation.tailrec

class TailCallsCrasher3[@miniboxed A] {
  @tailrec final def euclid2(a: A, b: A): A = a match {
    case _ => euclid2(a, b)
  }
}
