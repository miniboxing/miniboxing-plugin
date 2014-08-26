package miniboxing.tests.compile.bug122

class C[@miniboxed X] extends B[Long] {
  val neutralElement: Long = 0l
  def aggregate(a: Long, b: Long): Long = a + b
}

abstract class B[@miniboxed T] {
  def aggregate(a: T, b: T): T
  def neutralElement: T
}
