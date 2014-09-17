package miniboxing.tests.correctness.si6223

class Foo[@miniboxed A](val a: A) {
  def bar[@miniboxed B](f: A => B) = println(new Foo(f(a)).getClass)
}

object Test {
  def main(args:Array[String]) {
    new Foo(123).bar(_.toString)
    new Foo(123).bar(_.toInt)
    new Foo(123).bar(_.toFloat)
    new Foo(1.3).bar(_.toString)
    new Foo(1.3).bar(_.toInt)
    new Foo(1.3).bar(_.toFloat)
    new Foo("1").bar(_.toString)
    new Foo("1").bar(x => 3)
    new Foo("1").bar(x => 3.0)
  }
}
