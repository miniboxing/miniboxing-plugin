package miniboxing.tests.compile.bug83.alt2

class C[@miniboxed T](val t1: T, val t2: T){
  class E
  type Z = E
  def foo = ???
  def boo(t: T): T = t
  var t = t1
  t = t2
  val xxx = t
  println(t)
}

object Test {
  def main(args: Array[String]): Unit =
    new C(1, 3)
}
