class C[T, U] {
  def foo(t: T, u: U): Int = 1
}

class D[T, @miniboxed U] extends C[T, U] {
  override def foo(t: T, u: U): Int = 2
}

class E[@miniboxed T, @miniboxed U] extends D[T, U] {
  override def foo(t: T, u: U): Int = 3
}
