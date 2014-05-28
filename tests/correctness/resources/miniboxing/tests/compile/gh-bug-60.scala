package miniboxing.tests.compile.bug60

object Test1 {
  def foo[@miniboxed T](t: T): T = ???
  def bar[@miniboxed T](t: T): T = foo(t)
}  
