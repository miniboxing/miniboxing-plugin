package miniboxing.tests.compile.bug83.alt1

class C[@miniboxed T](val t1: T, val t2: T){
  var t = t1
  t = t2
  println(t)
}

object Test {
  def main(args: Array[String]): Unit =
    new C(1, 3)
}
