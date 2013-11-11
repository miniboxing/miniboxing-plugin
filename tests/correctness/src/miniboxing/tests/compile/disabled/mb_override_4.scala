package miniboxing.tests.compile


class Base41[T, S] {
  def overrideMe(t1: T, t2: T, t3: T, s: S): T = t1
}

class Base42[@miniboxed T, S] extends Base41[T, S] {
  override def overrideMe(t1: T, t2: T, t3: T, s: S): T = t2
}

class Base43[@miniboxed T, @miniboxed S] extends Base42[T, S] {
  override def overrideMe(t1: T, t2: T, t3: T, s: S): T = t3
}
