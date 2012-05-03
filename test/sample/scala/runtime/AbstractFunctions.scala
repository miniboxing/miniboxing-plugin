package scala.runtime

abstract class AbstractFunction0[@specialized +R] extends Function0[R] {
  //println("mine")
}

abstract class AbstractFunction1[@specialized -T, @specialized +R] extends Function1[T, R] {
  //println("mine")
}

abstract class AbstractFunction2[@specialized -T1 ,@specialized -T2, +R] extends Function2[T1, T2, R] {
  //println("mine")
}
