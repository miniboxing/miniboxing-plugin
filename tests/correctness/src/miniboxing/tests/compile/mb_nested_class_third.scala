package miniboxing.tests.compile.nested



class CCC[@miniboxed T](c: T) {
  class DDD[@miniboxed U](d: U) {
    def foo() = println(c == d)
  }
  def test() = {
    new DDD(c).foo
  }
}
