package miniboxing.tests.compile
import miniboxing.plugin.minispec

trait Base31[@minispec T, @minispec S] {
  def overrideMe(t1: T, t2: T, t3: T, s: S): T = t1
}

trait Base32[@minispec T, @minispec S] extends Base31[T, S] {
  override def overrideMe(t1: T, t2: T, t3: T, s: S): T = t2
}

trait Base33[@minispec T, @minispec S] extends Base32[T, S] {
  override def overrideMe(t1: T, t2: T, t3: T, s: S): T = t3
}
