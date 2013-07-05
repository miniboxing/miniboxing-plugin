package miniboxing.tests.compile
import miniboxing.plugin.minispec
import scala.annotation.tailrec

class TailCallsCrasher[@minispec A] {
  @tailrec final def zzz1(x: A): A = zzz1(x)
  @tailrec private[this] def zzz2(x: A): A = zzz2(x)
}
