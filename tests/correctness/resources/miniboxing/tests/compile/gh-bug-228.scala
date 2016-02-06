
trait Foo {
  def foo(f: => Int): Foo
  override def toString() = "foo"
}

class Bar extends Foo {
  def foo(f: => Int): Bar =
    this
  override def toString() = "bar"
}

object Test {
  def main(args: Array[String]): Unit = {
    val foo: Foo = new Bar()
    println(foo.foo(3))
  }
}
