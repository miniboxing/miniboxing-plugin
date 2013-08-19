package miniboxing.tests.compile.library6


package A {
  trait Iterator[@miniboxed +A]
}

package B {
  trait Iterator[@miniboxed +A]
}

class Iterator[@miniboxed +A] extends A.Iterator[A] with B.Iterator[A]
