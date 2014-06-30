package miniboxing.tests.compile.bug111

class Foo[@miniboxed T] {
  def foo(x: T) = {
    def applyOrElse[A1 <: Unit, B1 >: Unit](x1: A1, default: A1 => B1): B1 = x1.asInstanceOf[Unit] match {
      case () => ()
      case _ => default.apply(x1)
    }
  }
}
