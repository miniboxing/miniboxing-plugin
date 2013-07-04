package miniboxing.tests.compile
import miniboxing.plugin.minispec

trait Order2[@minispec A] {
  def max(x: A, y: A): A
}

final class ArrayOps[@minispec A](arr:Array[A]) {
  def qmax(implicit ev:Order2[A]) = {
    def f5(t:A): A = f5(ev.max(t, arr(0)))
    f5(arr(0))
  }
}
