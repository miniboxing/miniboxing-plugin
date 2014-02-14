package miniboxing.tests.compile.duplicator.tpars1

/** This test makes sure the duplicator also
    invalidates type parameter definitions */
class CCCCC[@miniboxed T](c: T) {
  def test(): Any = {
    class DDDDD[U](d: U) {
      def foo = ???
    }
    new DDDDD(c)
  }
}

/** This is exactly SI-7626 */
class C[@miniboxed T](c: T) {
  def test(t: T): Any = {
    class D[U](d: U)
    new D(c)
  }
}
