package miniboxing.tests.compile.bug150

object Test extends App {
  trait C[T] {
    def compare(t1: T, t2: T): Boolean
  }

  def foo[@miniboxed T](t1: T, t2: T) = {
    val c =
      new C[T] {
        def compare(t1: T, t2: T): Boolean = t1 == t2
        def foo: T = ???
      }
    println(c.compare(t1, t2))
    c
  }

  foo(300, 400)
  foo(3.0, 3.0)
}
