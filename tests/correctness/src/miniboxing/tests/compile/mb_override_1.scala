package miniboxing.tests.compile


class Base1[@miniboxed T, @miniboxed S] {
  def overrideMe(t: T, s: S): T = t
}

class Override11[T, S] extends Base1[T, S] {
  override def overrideMe(t: T, s: S): T = ???
}

class Override12[T] extends Base1[T, Int] {
  override def overrideMe(t: T, s: Int): T = ???
}

class Override13 extends Base1[Int, Int] {
  override def overrideMe(t: Int, s: Int): Int = ???
}

// Other things to try:
//  - overriding with minispec-annotated methods
//  - overriding with normalized members
