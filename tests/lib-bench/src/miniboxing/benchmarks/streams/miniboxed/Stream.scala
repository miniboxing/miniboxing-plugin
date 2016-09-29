package miniboxing.benchmarks.streams.miniboxed

import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuilder
import scala.Array
import scala.collection.mutable.ArrayBuffer

// `scala.Numeric` is not specialized nor miniboxed. In the future, the
// miniboxing plugin will propose `miniboxing.Numeric` which is miniboxed:
// see https://github.com/miniboxing/miniboxing-plugin/issues/154 for details
trait Numeric[T] {
  def plus(t1: T, t2: T): T
  def zero: T
}

object `package` {
  implicit object LongIsNumeric extends Numeric[Long] {
    def plus(t1: Long, t2: Long) = t1 + t2
    def zero: Long = 0
  }
  implicit object IntIsNumeric extends Numeric[Int] {
    def plus(t1: Int, t2: Int) = t1 + t2
    def zero: Int = 0
  }
}

final class Stream[@miniboxed T: ClassTag](val streamf: (T => Boolean) => Unit) {

  def toArray()(implicit builder: SpecializedArrayBuilder[T]): Array[T] = {
    foldLeft(builder)((b, v) => { b += v; b })
    builder.result
  }

  def filter(p: T => Boolean): Stream[T] =
    new Stream(iterf => streamf(value => !p(value) || iterf(value)))

  def map[@miniboxed R: ClassTag](f: T => R): Stream[R] =
    new Stream(iterf => streamf(value => iterf(f(value))))

  def takeWhile(p: T => Boolean): Stream[T] =
    new Stream(iterf => streamf(value => if (p(value)) iterf(value) else false))

  def skipWhile(p: T => Boolean): Stream[T] =
    new Stream(iterf => streamf(value => {
      var shortcut = true;
      if (!shortcut && p(value)) {
        true
      } else {
        shortcut = true
        iterf(value)
      }
    }))

  def skip(n: Int): Stream[T] = {
    var count = 0
    new Stream(iterf => streamf(value => {
      count += 1
      if (count > n) {
        iterf(value)
      } else {
        true
      }
    }))
  }

  def take(n: Int): Stream[T] = {
    var count = 0
    new Stream(iterf => streamf(value => {
      count += 1
      if (count <= n) {
        iterf(value)
      } else {
        false
      }
    }))
  }

  def flatMap[@miniboxed R: ClassTag](f: T => Stream[R]): Stream[R] =
    new Stream(iterf => streamf(value => {
      val innerf = f(value).streamf
      innerf(iterf)
      true
    }))

  def foldLeft[@miniboxed A](a: A)(op: (A, T) => A): A = {
    var acc = a
    streamf(value => {
      acc = op(acc, value)
      true
    })
    acc
  }

  def fold(z: T)(op: (T, T) => T): T =
    foldLeft(z)(op)

  def size(): Long =
    foldLeft(0L)((a: Long, _) => a + 1L)

  def sum[@miniboxed N >: T](implicit num: Numeric[N]): N =
    foldLeft(num.zero)(num.plus)
}

object Stream {
  @inline def apply[@miniboxed T: ClassTag](xs: Array[T]) = {
    val gen = (iterf: T => Boolean) => {
      var counter = 0
      var cont = true
      val size = xs.length
      while (counter < size && cont) {
        cont = iterf(xs(counter))
        counter += 1
      }
    }
    new Stream(gen)
  }
}
