package miniboxing.test.compile.bugs63

class C[@miniboxed T](val t: T)

object Test {
  println(new C[Unit](()).t)
  println(new C[Boolean](false).t)
  println(new C[Byte](3).t)
  println(new C[Char]('c').t)
  println(new C[Short](3).t)
  println(new C[Int](3).t)
  println(new C[Long](3).t)
  println(new C[Float](3).t)
  println(new C[Double](3).t)
  println(new C[Nothing](???).t)
}
