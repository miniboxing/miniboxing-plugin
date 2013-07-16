package miniboxing.tests.compile.library6
import miniboxing.plugin.minispec

package A {
  trait Iterator[@minispec +A]
}

package B {
  trait Iterator[@minispec +A]
}

class Iterator[@minispec +A] extends A.Iterator[A] with B.Iterator[A]
