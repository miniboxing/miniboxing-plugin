object Test {
  def test[@miniboxed T](a: Array[T]) =
    // in the miniboxed variants of the test method this
    // code should be rewired to mbarray_{apply, update}:
    a(0) = a(1)
}
