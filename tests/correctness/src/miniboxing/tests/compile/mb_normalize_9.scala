package miniboxing.tests.compile


class Norm1 {
  def overrideMe[@miniboxed A, @miniboxed B, @miniboxed C](a: A, b: B, c: C): A = a
}

class Norm2 extends Norm1 {
  override def overrideMe[@miniboxed A, B, @miniboxed C](a: A, b: B, c: C): A = ???
}
