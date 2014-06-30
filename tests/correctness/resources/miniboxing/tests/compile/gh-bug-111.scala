package miniboxing.tests.compile.bug111

// This bug corresponds to:
// https://issues.scala-lang.org/browse/SI-8633 and 
// https://issues.scala-lang.org/browse/SI-8694
class Foo[@miniboxed T] {
  def foo(x: T) = {
    val eh: PartialFunction[Unit, Unit] = {
      case () => ()
    }
  }
}
