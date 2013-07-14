package miniboxing.tests.compile
import miniboxing.plugin.minispec
import scala.annotation.tailrec

class TailCallsCrasher2[@minispec A] {
  @tailrec final def euclid1(a: A, b: A): A =
    if (b == 0) a else euclid1(b, b)
}
