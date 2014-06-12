package miniboxing.tests.compile.bug69.red1
 
object Test {
  def foo = {
    class C[@miniboxed T]
    class D
    ???
  }
}
