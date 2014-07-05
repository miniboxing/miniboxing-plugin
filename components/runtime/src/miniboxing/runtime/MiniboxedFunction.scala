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
  def f: Function1[T1, R]
  def apply(t1: T1): R
}

/**
 * A miniboxed version of the Function2. It is used to
 * efficiently invoke the function from a miniboxed environment.
 */
trait MiniboxedFunction2[@miniboxed -T1, @miniboxed -T2, @miniboxed +R] {
  def f: Function2[T1, T2, R]
  def apply(t1: T1, t2: T2): R
}