package miniboxing.runtime

import scala.runtime.{AbstractFunction0, AbstractFunction1, AbstractFunction2}

/**
 * A miniboxed version of the Function0. It is used to
 * efficiently invoke the function from a miniboxed environment.
 */
trait MiniboxedFunction0[@miniboxed +R] {
  def extractFunctionX: Function0[R]
  def apply(): R
  override def toString = "<function0>"
}

/**
 * A miniboxed version of the Function1. It is used to
 * efficiently invoke the function from a miniboxed environment.
 */
trait MiniboxedFunction1[@miniboxed -T1, @miniboxed +R] {
  self =>

  def extractFunctionX: Function1[T1, R]
  def apply(t1: T1): R
  override def toString = "<function1>"

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
  def extractFunctionX: Function2[T1, T2, R]
  def apply(t1: T1, t2: T2): R
  override def toString = "<function2>"

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

abstract class AbstractFunction0Wrapper[+R] extends AbstractFunction0[R] {
  def m: MiniboxedFunction0[R]
}

abstract class AbstractFunction1Wrapper[-T1, +R] extends AbstractFunction1[T1, R] {
  def m: MiniboxedFunction1[T1, R]
}

abstract class AbstractFunction2Wrapper[-T1, -T2, +R] extends AbstractFunction2[T1, T2, R] {
  def m: MiniboxedFunction2[T1, T2, R]
}

// AbstractMiniboxedFunction{0,1,2} needs to extend Serializable, else we run into #161
// background: http://stackoverflow.com/questions/12125076/java-deserialization-invalidclassexception-no-valid-constructor
abstract class AbstractMiniboxedFunction0[@miniboxed +R] extends MiniboxedFunction0[R] with Serializable {
  val extractFunctionX: Function0[R] = new AbstractFunction0Wrapper[R] with Serializable {
    def apply(): R =  AbstractMiniboxedFunction0.this.apply()
    def m: MiniboxedFunction0[R] = AbstractMiniboxedFunction0.this
  }
  def apply(): R
}

abstract class AbstractMiniboxedFunction1[@miniboxed -T1, @miniboxed +R] extends MiniboxedFunction1[T1, R] with Serializable {
  val extractFunctionX: Function1[T1, R] = new AbstractFunction1Wrapper[T1, R] with Serializable {
    def apply(t1: T1): R =  AbstractMiniboxedFunction1.this.apply(t1)
    def m: MiniboxedFunction1[T1, R] = AbstractMiniboxedFunction1.this
  }
  def apply(t1: T1): R
}

abstract class AbstractMiniboxedFunction2[@miniboxed -T1, @miniboxed -T2, @miniboxed +R] extends MiniboxedFunction2[T1, T2, R] with Serializable {
  val extractFunctionX: Function2[T1, T2, R] = new AbstractFunction2Wrapper[T1, T2, R] with Serializable {
    def apply(t1: T1, t2: T2): R =  AbstractMiniboxedFunction2.this.apply(t1, t2)
    def m: MiniboxedFunction2[T1, T2, R] = AbstractMiniboxedFunction2.this
  }
  def apply(t1: T1, t2: T2): R
}
