package miniboxing.tests.compile.bug69
 
object Test {
  def foo = {
    class C[@miniboxed T]
    ???
  }
}
