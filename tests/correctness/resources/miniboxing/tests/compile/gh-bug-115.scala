package miniboxing.tests.bug115

object Test {
  def foo(f: Int => Int @api): Unit = println(f(3))
  def bar(f: Int => Int): Unit = foo(f)
  def baz1(x: => Int @api): Unit = println(x)
  def baz2(x: => Int @api): Unit = ()
  def zab1(x: => Int): Unit = println(x)
  def zab2(x: => Int): Unit = ()

  def main(args: Array[String]): Unit = {
    foo((x: Int) => x + 1)
    bar((x: Int) => x + 1)
    val f: Int => Int @api =
      (x: Int) => x + 1
    foo(f)
    bar(f)

    baz1({ println("eval1"); 1})
    baz2({ println("eval2"); 2})
    zab1({ println("eval3"); 3})
    zab2({ println("eval4"); 4})
  }

}
