package miniboxing.tests.compile
import miniboxing.plugin.minispec

class Base41[T, S] {
  def overrideMe(t1: T, t2: T, t3: T, s: S): T = t1
}

class Base42[@minispec T, S] extends Base41[T, S] {
  override def overrideMe(t1: T, t2: T, t3: T, s: S): T = t2
}

class Base43[@minispec T, @minispec S] extends Base42[T, S] {
  override def overrideMe(t1: T, t2: T, t3: T, s: S): T = t3
}
