package miniboxing.tests.compile.bug34
import miniboxing.plugin.minispec

trait Function2[@minispec -T1, @minispec -T2, @minispec +R] extends AnyRef { self =>
  def apply(v1: T1, v2: T2): R
  def curried: T1 => T2 => R = {
    (x1: T1) => (x2: T2) => apply(x1, x2)
  }
  def tupled: Tuple2[T1, T2] => R = {
    case Tuple2(x1, x2) => apply(x1, x2)
  }
  override def toString() = "<function2>"
}
