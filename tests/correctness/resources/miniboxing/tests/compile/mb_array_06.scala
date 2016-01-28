class Test[@miniboxed T]{
  def foo(a: Array[T]) = {
    // in the miniboxed classes, these should be rewritten:
    a(0) = a(1)
  }
}
