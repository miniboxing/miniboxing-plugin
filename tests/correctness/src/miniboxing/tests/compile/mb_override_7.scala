package miniboxing.tests.compile
import miniboxing.plugin.minispec

class Base71 {
  def overrideMe[@minispec A, @minispec B](a: A, b: B): A = a
}

class Base72 extends Base71 {
  override def overrideMe[@minispec A, B](a: A, b: B): A = ???
}
