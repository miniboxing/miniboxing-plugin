package miniboxing.runtime

object MiniboxedFunctionBridge {

  def function0_bridge[R](_f: Function0[R]): MiniboxedFunction0[R] =
    new MiniboxedFunction0[R] {
      def f = _f
      def apply(): R = _f()
    }

  def function1_bridge[T, R](_f: Function1[T, R]): MiniboxedFunction1[T, R] =
    new MiniboxedFunction1[T, R] {
      def f = _f
      def apply(t: T): R = _f(t)
    }

  def function2_bridge[T1, T2, R](_f: Function2[T1, T2, R]): MiniboxedFunction2[T1, T2, R] =
    new MiniboxedFunction2[T1, T2, R] {
      def f = _f
      def apply(t1: T1, t2: T2): R = _f(t1, t2)
    }

  // TODO: Specialized bridges
}