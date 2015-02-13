package a

final class Stream[@miniboxed T](val streamf: (T => Boolean) => Unit) {
  def filter(): Stream[T] =
    new Stream(iterf => streamf(value => iterf(value)))
}
