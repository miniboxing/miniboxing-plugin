package miniboxing.tests.bug115

object Test {
  def foo(f: (Int => Int) @api): Unit = println(f(3))
  def bar(f: Int => Int): Unit = foo(f)

  def main(args: Array[String]): Unit = {
    foo((x: Int) => x + 1)
    bar((x: Int) => x + 1)
    val f: (Int => Int) @api =
      (x: Int) => x + 1
    foo(f)
    bar(f)
  }
}
