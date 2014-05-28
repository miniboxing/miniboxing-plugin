package miniboxing.tests.compile.bug88

class CCC[@miniboxed T](val c: T) {
  class DDD[@miniboxed U](val d: U) {
    def foo() = println(c == d)
  }
}
