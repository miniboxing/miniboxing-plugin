package miniboxing.tests.compile.nested



class CC[@miniboxed T](c: T) {
  class DD[@miniboxed U](d: U)
  def test = {
    new DD(c)
  }
}
