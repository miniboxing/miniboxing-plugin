package miniboxing.tests.compile.bug149
import scala.reflect.ClassTag

trait MbFunction1[@miniboxed -T1, @miniboxed +R] {
  def apply(t1: T1): R
}

final class Stream[@miniboxed T: ClassTag](val streamf: MbFunction1[MbFunction1[T, Boolean], Unit]) {

  def filter(p: MbFunction1[T, Boolean]): Stream[T] =
    new Stream(
      new MbFunction1[MbFunction1[T, Boolean], Unit] {
        def apply(iterf: MbFunction1[T, Boolean]) =
          streamf(new MbFunction1[T, Boolean] {
            def apply(value: T): Boolean = !p(value) || iterf(value)
          })
      })
}
