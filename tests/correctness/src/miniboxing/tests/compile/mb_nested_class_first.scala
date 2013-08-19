package miniboxing.tests.compile.nested



class C[@miniboxed T](c: T) {
  class D[@miniboxed U](d: U)
}
