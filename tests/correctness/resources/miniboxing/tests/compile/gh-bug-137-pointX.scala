package miniboxing.tests.compile.bug137.pointX

object Test {
  def foo[F](t: F): F = t
  class A[A](t: A)

  // now, expecting warnings:
  class C[@miniboxed T] {
    def bar(t: T) = {
      foo[T](t)
      foo[Any](t)
      new A[T](t)
      new A[Any](t)
    }
  }

  def baz[@miniboxed X](x: X) = {
    foo(x)
    new A(x)
  }

  class D[@miniboxed X] {
    def buz[@miniboxed Y](x: X, y: Y) = {
       new A(y)
       new A(x)
    }
  }

  foo(3)
  new A(3)

  foo(3.0)
  new A(3.0)

  foo("x")
  new A("x")
}
