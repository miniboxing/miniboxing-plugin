package miniboxing.runtime

/**
 * A miniboxed version of the Function0. It is used to
 * efficiently invoke the function from a miniboxed environment.
 */
trait MiniboxedFunction0[@miniboxed +R] {
  def f: Function0[R]
  def apply(): R
}

/**
 * A miniboxed version of the Function1. It is used to
 * efficiently invoke the function from a miniboxed environment.
 */
trait MiniboxedFunction1[@miniboxed -T1, @miniboxed +R] {
  self =>

  def f: Function1[T1, R]
  def apply(t1: T1): R

//  def compose[@miniboxed A](g: MiniboxedFunction1[A, T1]): MiniboxedFunction1[A, R] =
//    new AbstractMiniboxedFunction1[A, R] {
//      def apply(a: A): R = self.apply(g.apply(a))
//    }
//
//  def andThen[@miniboxed A](g: MiniboxedFunction1[R, A]): MiniboxedFunction1[T1, A] =
//    new AbstractMiniboxedFunction1[T1, A] {
//      def apply(x: T1): A = g.apply(self.apply(x))
//    }
}

/**
 * A miniboxed version of the Function2. It is used to
 * efficiently invoke the function from a miniboxed environment.
 */
trait MiniboxedFunction2[@miniboxed -T1, @miniboxed -T2, @miniboxed +R] {
  def f: Function2[T1, T2, R]
  def apply(t1: T1, t2: T2): R

//  def curried: MiniboxedFunction1[T1, Function1[T2, R]] =
//    new AbstractMiniboxedFunction1[T1, Function1[T2, R]] {
//      def apply(x1: T1) = (x2: T2) => MiniboxedFunction2.this.apply(x1, x2)
//    }
//
//  def tupled: MiniboxedFunction1[Tuple2[T1, T2], R] =
//    new AbstractMiniboxedFunction1[Tuple2[T1, T2], R] {
//      def apply(tup: Tuple2[T1, T2]): R =
//        MiniboxedFunction2.this.apply(tup._1, tup._2)
//    }
}

abstract class AbstractMiniboxedFunction0[@miniboxed +R] extends MiniboxedFunction0[R] {
  def f: Function0[R] = () => apply()
  def apply(): R
}

abstract class AbstractMiniboxedFunction1[@miniboxed -T1, @miniboxed +R] extends MiniboxedFunction1[T1, R] {
  def f: Function1[T1, R] = (x: T1) => apply(x)
  def apply(t1: T1): R
}

abstract class AbstractMiniboxedFunction2[@miniboxed -T1, @miniboxed -T2, @miniboxed +R] extends MiniboxedFunction2[T1, T2, R] {
  def f: Function2[T1, T2, R] = (x1: T1, x2: T2) => apply(x1, x2)
  def apply(t1: T1, t2: T2): R
}
