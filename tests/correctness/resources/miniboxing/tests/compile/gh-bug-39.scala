package miniboxing.tests.compile.bug39

trait X[@miniboxed +A] {
  def min[A1 >: A](): A
}
