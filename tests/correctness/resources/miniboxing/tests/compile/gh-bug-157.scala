package miniboxin.tests.compile.bug157

abstract class Foo {
  protected val foo: Boolean
}

object Test {
  def foo[@miniboxed T](t: T) = 
    new Foo {
      protected val foo: Boolean = true
    }
}
