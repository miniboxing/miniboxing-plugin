package miniboxing.tests.compile.bug93
 
object Test {
  def bar = {
    def baz[@miniboxed T](t: T): T = t
  }
}
