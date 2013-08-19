package miniboxing.tests.compile


class Base61[@miniboxed T, @miniboxed S] {
  def overrideMe[@miniboxed C](t1: T, t2: T, s: S, c: C): T = t1
}

class Base62[@miniboxed T, S] extends Base61[T, S] {
  override def overrideMe[@miniboxed C](t1: T, t2: T, s: S, c: C): T = t2
}
