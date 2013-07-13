package miniboxing.tests.compile
import miniboxing.plugin.minispec

class Base61[@minispec T, @minispec S] {
  def overrideMe[@minispec C](t1: T, t2: T, s: S, c: C): T = t1
}

class Base62[@minispec T, S] extends Base61[T, S] {
  override def overrideMe[@minispec C](t1: T, t2: T, s: S, c: C): T = t2
}
