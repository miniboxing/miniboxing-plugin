package miniboxing.tests.compile


class Base71 {
  def overrideMe[@miniboxed A, @miniboxed B](a: A, b: B): A = a
}

class Base72 extends Base71 {
  override def overrideMe[@miniboxed A, B](a: A, b: B): A = ???
}
