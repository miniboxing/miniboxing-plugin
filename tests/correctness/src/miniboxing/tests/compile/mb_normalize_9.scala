package miniboxing.tests.compile
import miniboxing.plugin.minispec

class Norm1 {
  def overrideMe[@minispec A, @minispec B, @minispec C](a: A, b: B, c: C): A = a
}

class Norm2 extends Norm1 {
  override def overrideMe[@minispec A, B, @minispec C](a: A, b: B, c: C): A = ???
}
