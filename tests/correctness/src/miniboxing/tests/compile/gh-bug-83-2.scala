package miniboxing.tests.compile.bug83.alt2

class C[@miniboxed T](val t1: T, val t2: T){
  class D
  type Z = D
  object O
  def foo = ???
  def boo(t: T): T = t
  val xxx = t
  var t = t1
  t = t2
  println(t)
}

object Test {
  def main(args: Array[String]): Unit =
    new C(1, 3)
}
