package miniboxing.tests.compile


class Base51[@miniboxed T, @miniboxed S] {
  def overrideMe(t1: T, t2: T, t3: T, s: S): T = t1
}

class Base52[@miniboxed T, S] extends Base51[T, S] {
  override def overrideMe(t1: T, t2: T, t3: T, s: S): T = t2
}

class Base53[T, S] extends Base52[T, S] {
  override def overrideMe(t1: T, t2: T, t3: T, s: S): T = t3
}
