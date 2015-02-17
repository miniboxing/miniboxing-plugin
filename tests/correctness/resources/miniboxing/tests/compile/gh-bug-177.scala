package miniboxing.tests.compile.bug177

object BridgeTest {

 trait T[X] {
    def foo(cp: X): X = ???
  }

  trait U extends T[Int => Int] {
    override def foo(cp: Int => Int): Int => Int = cp
  }
}
