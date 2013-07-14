package miniboxing.tests.compile
import miniboxing.plugin.minispec
import scala.annotation.tailrec

class TailCallsCrasher3[@minispec A] {
  final def euclid2(a: A, b: A): A = a match {
    case _ => euclid2(a, b)
  }
}
