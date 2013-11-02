package miniboxing.tests.compile

import scala.annotation.tailrec

class TailCallsCrasher2[@miniboxed A] {
  @tailrec final def euclid1(a: A, b: A): A =
    if (b == 0) a else euclid1(b, b)
}
