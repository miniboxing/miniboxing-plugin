package miniboxing.correctness.test

class Z[T] {
  def foo(t: T) = t
  def bar(t: T) = t
  val baz: T = ???
}

class C[@miniboxing.plugin.minispec T] extends Z[T] {

  override def foo(t: T) = {
    println("overridden foo")
    bar(t)
    baz
  }

  def boo(t: T): T = this.boo(t)
}
