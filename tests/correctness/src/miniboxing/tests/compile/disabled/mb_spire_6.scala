package miniboxing.tests.compile

import scala.annotation.tailrec

class TailCallsCrasher[@miniboxed A] {
  @tailrec final def zzz1(x: A): A = zzz1(x)
  @tailrec private[this] def zzz2(x: A): A = zzz2(x)
}
