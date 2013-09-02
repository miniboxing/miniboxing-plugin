package miniboxing.tests.compile


trait Base31[@miniboxed T, @miniboxed S] {
  def overrideMe(t1: T, t2: T, t3: T, s: S): T = t1
}

trait Base32[@miniboxed T, @miniboxed S] extends Base31[T, S] {
  override def overrideMe(t1: T, t2: T, t3: T, s: S): T = t2
}

trait Base33[@miniboxed T, @miniboxed S] extends Base32[T, S] {
  override def overrideMe(t1: T, t2: T, t3: T, s: S): T = t3
}
