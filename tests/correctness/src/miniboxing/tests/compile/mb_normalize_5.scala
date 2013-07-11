package miniboxing.tests.correctness
import miniboxing.plugin.minispec

class Sp5Tuple2[@minispec U, @minispec V](u: U, v: V)

object Sp5Test {
  def normalize[@minispec S, @minispec T](s: S, t: T) = {
    def foo = {
      def bar = {
        new Sp5Tuple2(s, t)
      }
      bar
    }
    foo
  }
}
