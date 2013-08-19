package miniboxing.tests.correctness


class Sp5Tuple2[@miniboxed U, @miniboxed V](u: U, v: V)

object Sp5Test {
  def normalize[@miniboxed S, @miniboxed T](s: S, t: T) = {
    def foo = {
      def bar = {
        new Sp5Tuple2(s, t)
      }
      bar
    }
    foo
  }
}
