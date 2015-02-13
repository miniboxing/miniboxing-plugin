package miniboxing.tests.compile.streams
import scala.reflect.ClassTag

final class Stream[@miniboxed T: ClassTag](val streamf: (T => Boolean) => Unit) {
  def filter(p: T => Boolean): Stream[T] =
    new Stream(iterf => streamf(value => !p(value) || iterf(value)))
}
