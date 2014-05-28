package miniboxing.tests.compile.bug69.red2
 
object Test {
  def foo[@miniboxed T](t: T): T = {
    def bar[@miniboxed U](u: U): U = {
      def baz[@miniboxed V](v: V): V = v
      baz(u)
    }
    bar(t)
  }
}
