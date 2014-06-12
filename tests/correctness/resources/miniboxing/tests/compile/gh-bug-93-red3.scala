package miniboxing.tests.compile.bug69.red3
 
object Test {
  def foo[@miniboxed T](t: T): T = {
    def bar[@miniboxed U](u: U): U = u
    bar(t)
  }
}
