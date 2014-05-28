package miniboxing.tests.compile.bug89

class C[@miniboxed T] {
  def foo1(t: T) = C.foo_impl1
  def foo2(t: T) = ??? // C.foo_impl2
  def foo3(t: T) = C.foo_impl3
  def foo4(t: T) = C.foo_impl4
  def foo5(t: T) = C.foo_impl5
}

object C {
  private[compile] def foo_impl1 = ???
  private[this]    def foo_impl2 = ???
  private[C]       def foo_impl3 = ???
  private          def foo_impl4 = ???
  protected        def foo_impl5 = ???
}
