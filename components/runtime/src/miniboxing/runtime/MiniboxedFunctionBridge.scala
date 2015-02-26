//
//     _____   .__         .__ ___.                    .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_ |__    ____  ___  ___|__|  ____     ____
//   /  \ /  \ |  | /    \ |  | | __ \  /  _ \ \  \/  /|  | /    \   / ___\
//  /    Y    \|  ||   |  \|  | | \_\ \(  <_> ) >    < |  ||   |  \ / /_/  >
//  \____|__  /|__||___|  /|__| |___  / \____/ /__/\_ \|__||___|  / \___  /
//          \/          \/          \/               \/         \/ /_____/
// Copyright (c) 2011-2015 Scala Team, École polytechnique fédérale de Lausanne
//
// Authors:
//    * Vlad Ureche
//
package miniboxing.runtime

object MiniboxedFunctionBridge {

  def function0_bridge[R](_f: Function0[R]): MiniboxedFunction0[R] =
    if (_f.isInstanceOf[AbstractFunction0Wrapper[_]])
      _f.asInstanceOf[AbstractFunction0Wrapper[_]].m.asInstanceOf[MiniboxedFunction0[R]]
    else
      new MiniboxedFunction0[R] {
        def extractFunctionX: Function0[R] = _f
        def apply(): R = _f()
      }

  def function1_bridge[T1, R](_f: Function1[T1, R]): MiniboxedFunction1[T1, R] =
    if (_f.isInstanceOf[AbstractFunction1Wrapper[_, _]])
      _f.asInstanceOf[AbstractFunction1Wrapper[_, _]].m.asInstanceOf[MiniboxedFunction1[T1, R]]
    else
      new MiniboxedFunction1[T1, R] {
        def extractFunctionX: Function1[T1, R] = _f
        def apply(t: T1): R = _f(t)
      }

  def function2_bridge[T1, T2, R](_f: Function2[T1, T2, R]): MiniboxedFunction2[T1, T2, R] =
    if (_f.isInstanceOf[AbstractFunction2Wrapper[_, _, _]])
      _f.asInstanceOf[AbstractFunction2Wrapper[_, _, _]].m.asInstanceOf[MiniboxedFunction2[T1, T2, R]]
    else
      new MiniboxedFunction2[T1, T2, R] {
        def extractFunctionX: Function2[T1, T2, R] = _f
        def apply(t1: T1, t2: T2): R = _f(t1, t2)
      }

  // Specialized bridges:
  def function0_opt_bridge_long[R](R_Tag: Byte, _f: Function0[R]): MiniboxedFunction0[R] =
    if (_f.isInstanceOf[AbstractFunction0Wrapper[_]])
      _f.asInstanceOf[AbstractFunction0Wrapper[_]].m.asInstanceOf[MiniboxedFunction0[R]]
    else
      ((R_Tag) match {
        case 0 =>
          val _f_cast = _f.asInstanceOf[Function0[Unit]]
          new MiniboxedFunction0[Unit] {
            def extractFunctionX: Function0[Unit] = _f_cast
            def apply(): Unit = _f_cast.apply()
          }
        case 1 =>
          val _f_cast = _f.asInstanceOf[Function0[Boolean]]
          new MiniboxedFunction0[Boolean] {
            def extractFunctionX: Function0[Boolean] = _f_cast
            def apply(): Boolean = _f_cast.apply()
          }
        case 2 =>
          val _f_cast = _f.asInstanceOf[Function0[Byte]]
          new MiniboxedFunction0[Byte] {
            def extractFunctionX: Function0[Byte] = _f_cast
            def apply(): Byte = _f_cast.apply()
          }
        case 3 =>
          val _f_cast = _f.asInstanceOf[Function0[Short]]
          new MiniboxedFunction0[Short] {
            def extractFunctionX: Function0[Short] = _f_cast
            def apply(): Short = _f_cast.apply()
          }
        case 4 =>
          val _f_cast = _f.asInstanceOf[Function0[Char]]
          new MiniboxedFunction0[Char] {
            def extractFunctionX: Function0[Char] = _f_cast
            def apply(): Char = _f_cast.apply()
          }
        case 5 =>
          val _f_cast = _f.asInstanceOf[Function0[Int]]
          new MiniboxedFunction0[Int] {
            def extractFunctionX: Function0[Int] = _f_cast
            def apply(): Int = _f_cast.apply()
          }
        case 6 =>
          val _f_cast = _f.asInstanceOf[Function0[Long]]
          new MiniboxedFunction0[Long] {
            def extractFunctionX: Function0[Long] = _f_cast
            def apply(): Long = _f_cast.apply()
          }
        case _ =>
          function0_bridge(_f)
      }).asInstanceOf[MiniboxedFunction0[R]]


  def function0_opt_bridge_double[R](R_Tag: Byte, _f: Function0[R]): MiniboxedFunction0[R] =
    if (_f.isInstanceOf[AbstractFunction0Wrapper[_]])
      _f.asInstanceOf[AbstractFunction0Wrapper[_]].m.asInstanceOf[MiniboxedFunction0[R]]
    else
      ((R_Tag) match {
        case 7 =>
          val _f_cast = _f.asInstanceOf[Function0[Float]]
          new MiniboxedFunction0[Float] {
            def extractFunctionX: Function0[Float] = _f_cast
            def apply(): Float = _f_cast.apply()
          }
        case 8 =>
          val _f_cast = _f.asInstanceOf[Function0[Double]]
          new MiniboxedFunction0[Double] {
            def extractFunctionX: Function0[Double] = _f_cast
            def apply(): Double = _f_cast.apply()
          }
        case _ =>
          function0_bridge(_f)
      }).asInstanceOf[MiniboxedFunction0[R]]


  def function1_opt_bridge_long_long[T, R](T_Tag: Byte, R_Tag: Byte, _f: Function1[T, R]): MiniboxedFunction1[T, R] =
    if (_f.isInstanceOf[AbstractFunction1Wrapper[_, _]])
      _f.asInstanceOf[AbstractFunction1Wrapper[_, _]].m.asInstanceOf[MiniboxedFunction1[T, R]]
    else
      ((T_Tag + R_Tag * 10) match {
        case 5 =>
          val _f_cast = _f.asInstanceOf[Function1[Int, Unit]]
          new MiniboxedFunction1[Int, Unit] {
            def extractFunctionX: Function1[Int, Unit] = _f_cast
            def apply(arg1: Int): Unit = _f_cast.apply(arg1)
          }
        case 15 =>
          val _f_cast = _f.asInstanceOf[Function1[Int, Boolean]]
          new MiniboxedFunction1[Int, Boolean] {
            def extractFunctionX: Function1[Int, Boolean] = _f_cast
            def apply(arg1: Int): Boolean = _f_cast.apply(arg1)
          }
        case 55 =>
          val _f_cast = _f.asInstanceOf[Function1[Int, Int]]
          new MiniboxedFunction1[Int, Int] {
            def extractFunctionX: Function1[Int, Int] = _f_cast
            def apply(arg1: Int): Int = _f_cast.apply(arg1)
          }
        case 65 =>
          val _f_cast = _f.asInstanceOf[Function1[Int, Long]]
          new MiniboxedFunction1[Int, Long] {
            def extractFunctionX: Function1[Int, Long] = _f_cast
            def apply(arg1: Int): Long = _f_cast.apply(arg1)
          }
        case 6 =>
          val _f_cast = _f.asInstanceOf[Function1[Long, Unit]]
          new MiniboxedFunction1[Long, Unit] {
            def extractFunctionX: Function1[Long, Unit] = _f_cast
            def apply(arg1: Long): Unit = _f_cast.apply(arg1)
          }
        case 16 =>
          val _f_cast = _f.asInstanceOf[Function1[Long, Boolean]]
          new MiniboxedFunction1[Long, Boolean] {
            def extractFunctionX: Function1[Long, Boolean] = _f_cast
            def apply(arg1: Long): Boolean = _f_cast.apply(arg1)
          }
        case 56 =>
          val _f_cast = _f.asInstanceOf[Function1[Long, Int]]
          new MiniboxedFunction1[Long, Int] {
            def extractFunctionX: Function1[Long, Int] = _f_cast
            def apply(arg1: Long): Int = _f_cast.apply(arg1)
          }
        case 66 =>
          val _f_cast = _f.asInstanceOf[Function1[Long, Long]]
          new MiniboxedFunction1[Long, Long] {
            def extractFunctionX: Function1[Long, Long] = _f_cast
            def apply(arg1: Long): Long = _f_cast.apply(arg1)
          }
        case _ =>
          function1_bridge(_f)
      }).asInstanceOf[MiniboxedFunction1[T, R]]


  def function1_opt_bridge_double_long[T, R](T_Tag: Byte, R_Tag: Byte, _f: Function1[T, R]): MiniboxedFunction1[T, R] =
    if (_f.isInstanceOf[AbstractFunction1Wrapper[_, _]])
      _f.asInstanceOf[AbstractFunction1Wrapper[_, _]].m.asInstanceOf[MiniboxedFunction1[T, R]]
    else
      ((T_Tag + R_Tag * 10) match {
        case 7 =>
          val _f_cast = _f.asInstanceOf[Function1[Float, Unit]]
          new MiniboxedFunction1[Float, Unit] {
            def extractFunctionX: Function1[Float, Unit] = _f_cast
            def apply(arg1: Float): Unit = _f_cast.apply(arg1)
          }
        case 17 =>
          val _f_cast = _f.asInstanceOf[Function1[Float, Boolean]]
          new MiniboxedFunction1[Float, Boolean] {
            def extractFunctionX: Function1[Float, Boolean] = _f_cast
            def apply(arg1: Float): Boolean = _f_cast.apply(arg1)
          }
        case 57 =>
          val _f_cast = _f.asInstanceOf[Function1[Float, Int]]
          new MiniboxedFunction1[Float, Int] {
            def extractFunctionX: Function1[Float, Int] = _f_cast
            def apply(arg1: Float): Int = _f_cast.apply(arg1)
          }
        case 67 =>
          val _f_cast = _f.asInstanceOf[Function1[Float, Long]]
          new MiniboxedFunction1[Float, Long] {
            def extractFunctionX: Function1[Float, Long] = _f_cast
            def apply(arg1: Float): Long = _f_cast.apply(arg1)
          }
        case 8 =>
          val _f_cast = _f.asInstanceOf[Function1[Double, Unit]]
          new MiniboxedFunction1[Double, Unit] {
            def extractFunctionX: Function1[Double, Unit] = _f_cast
            def apply(arg1: Double): Unit = _f_cast.apply(arg1)
          }
        case 18 =>
          val _f_cast = _f.asInstanceOf[Function1[Double, Boolean]]
          new MiniboxedFunction1[Double, Boolean] {
            def extractFunctionX: Function1[Double, Boolean] = _f_cast
            def apply(arg1: Double): Boolean = _f_cast.apply(arg1)
          }
        case 58 =>
          val _f_cast = _f.asInstanceOf[Function1[Double, Int]]
          new MiniboxedFunction1[Double, Int] {
            def extractFunctionX: Function1[Double, Int] = _f_cast
            def apply(arg1: Double): Int = _f_cast.apply(arg1)
          }
        case 68 =>
          val _f_cast = _f.asInstanceOf[Function1[Double, Long]]
          new MiniboxedFunction1[Double, Long] {
            def extractFunctionX: Function1[Double, Long] = _f_cast
            def apply(arg1: Double): Long = _f_cast.apply(arg1)
          }
        case _ =>
          function1_bridge(_f)
      }).asInstanceOf[MiniboxedFunction1[T, R]]


  def function1_opt_bridge_long_double[T, R](T_Tag: Byte, R_Tag: Byte, _f: Function1[T, R]): MiniboxedFunction1[T, R] =
    if (_f.isInstanceOf[AbstractFunction1Wrapper[_, _]])
      _f.asInstanceOf[AbstractFunction1Wrapper[_, _]].m.asInstanceOf[MiniboxedFunction1[T, R]]
    else
      ((T_Tag + R_Tag * 10) match {
        case 75 =>
          val _f_cast = _f.asInstanceOf[Function1[Int, Float]]
          new MiniboxedFunction1[Int, Float] {
            def extractFunctionX: Function1[Int, Float] = _f_cast
            def apply(arg1: Int): Float = _f_cast.apply(arg1)
          }
        case 85 =>
          val _f_cast = _f.asInstanceOf[Function1[Int, Double]]
          new MiniboxedFunction1[Int, Double] {
            def extractFunctionX: Function1[Int, Double] = _f_cast
            def apply(arg1: Int): Double = _f_cast.apply(arg1)
          }
        case 76 =>
          val _f_cast = _f.asInstanceOf[Function1[Long, Float]]
          new MiniboxedFunction1[Long, Float] {
            def extractFunctionX: Function1[Long, Float] = _f_cast
            def apply(arg1: Long): Float = _f_cast.apply(arg1)
          }
        case 86 =>
          val _f_cast = _f.asInstanceOf[Function1[Long, Double]]
          new MiniboxedFunction1[Long, Double] {
            def extractFunctionX: Function1[Long, Double] = _f_cast
            def apply(arg1: Long): Double = _f_cast.apply(arg1)
          }
        case _ =>
          function1_bridge(_f)
      }).asInstanceOf[MiniboxedFunction1[T, R]]


  def function1_opt_bridge_double_double[T, R](T_Tag: Byte, R_Tag: Byte, _f: Function1[T, R]): MiniboxedFunction1[T, R] =
    if (_f.isInstanceOf[AbstractFunction1Wrapper[_, _]])
      _f.asInstanceOf[AbstractFunction1Wrapper[_, _]].m.asInstanceOf[MiniboxedFunction1[T, R]]
    else
      ((T_Tag + R_Tag * 10) match {
        case 77 =>
          val _f_cast = _f.asInstanceOf[Function1[Float, Float]]
          new MiniboxedFunction1[Float, Float] {
            def extractFunctionX: Function1[Float, Float] = _f_cast
            def apply(arg1: Float): Float = _f_cast.apply(arg1)
          }
        case 87 =>
          val _f_cast = _f.asInstanceOf[Function1[Float, Double]]
          new MiniboxedFunction1[Float, Double] {
            def extractFunctionX: Function1[Float, Double] = _f_cast
            def apply(arg1: Float): Double = _f_cast.apply(arg1)
          }
        case 78 =>
          val _f_cast = _f.asInstanceOf[Function1[Double, Float]]
          new MiniboxedFunction1[Double, Float] {
            def extractFunctionX: Function1[Double, Float] = _f_cast
            def apply(arg1: Double): Float = _f_cast.apply(arg1)
          }
        case 88 =>
          val _f_cast = _f.asInstanceOf[Function1[Double, Double]]
          new MiniboxedFunction1[Double, Double] {
            def extractFunctionX: Function1[Double, Double] = _f_cast
            def apply(arg1: Double): Double = _f_cast.apply(arg1)
          }
        case _ =>
          function1_bridge(_f)
      }).asInstanceOf[MiniboxedFunction1[T, R]]


  def function2_opt_bridge_long_long_long[T1, T2, R](T1_Tag: Byte, T2_Tag: Byte, R_Tag: Byte, _f: Function2[T1, T2, R]): MiniboxedFunction2[T1, T2, R] =
    if (_f.isInstanceOf[AbstractFunction2Wrapper[_, _, _]])
      _f.asInstanceOf[AbstractFunction2Wrapper[_, _, _]].m.asInstanceOf[MiniboxedFunction2[T1, T2, R]]
    else
      ((T1_Tag + T2_Tag * 10 + R_Tag * 10 * 10) match {
        case 55 =>
          val _f_cast = _f.asInstanceOf[Function2[Int, Int, Unit]]
          new MiniboxedFunction2[Int, Int, Unit] {
            def extractFunctionX: Function2[Int, Int, Unit] = _f_cast
            def apply(arg1: Int, arg2: Int): Unit = _f_cast.apply(arg1, arg2)
          }
        case 155 =>
          val _f_cast = _f.asInstanceOf[Function2[Int, Int, Boolean]]
          new MiniboxedFunction2[Int, Int, Boolean] {
            def extractFunctionX: Function2[Int, Int, Boolean] = _f_cast
            def apply(arg1: Int, arg2: Int): Boolean = _f_cast.apply(arg1, arg2)
          }
        case 555 =>
          val _f_cast = _f.asInstanceOf[Function2[Int, Int, Int]]
          new MiniboxedFunction2[Int, Int, Int] {
            def extractFunctionX: Function2[Int, Int, Int] = _f_cast
            def apply(arg1: Int, arg2: Int): Int = _f_cast.apply(arg1, arg2)
          }
        case 655 =>
          val _f_cast = _f.asInstanceOf[Function2[Int, Int, Long]]
          new MiniboxedFunction2[Int, Int, Long] {
            def extractFunctionX: Function2[Int, Int, Long] = _f_cast
            def apply(arg1: Int, arg2: Int): Long = _f_cast.apply(arg1, arg2)
          }
        case 65 =>
          val _f_cast = _f.asInstanceOf[Function2[Int, Long, Unit]]
          new MiniboxedFunction2[Int, Long, Unit] {
            def extractFunctionX: Function2[Int, Long, Unit] = _f_cast
            def apply(arg1: Int, arg2: Long): Unit = _f_cast.apply(arg1, arg2)
          }
        case 165 =>
          val _f_cast = _f.asInstanceOf[Function2[Int, Long, Boolean]]
          new MiniboxedFunction2[Int, Long, Boolean] {
            def extractFunctionX: Function2[Int, Long, Boolean] = _f_cast
            def apply(arg1: Int, arg2: Long): Boolean = _f_cast.apply(arg1, arg2)
          }
        case 565 =>
          val _f_cast = _f.asInstanceOf[Function2[Int, Long, Int]]
          new MiniboxedFunction2[Int, Long, Int] {
            def extractFunctionX: Function2[Int, Long, Int] = _f_cast
            def apply(arg1: Int, arg2: Long): Int = _f_cast.apply(arg1, arg2)
          }
        case 665 =>
          val _f_cast = _f.asInstanceOf[Function2[Int, Long, Long]]
          new MiniboxedFunction2[Int, Long, Long] {
            def extractFunctionX: Function2[Int, Long, Long] = _f_cast
            def apply(arg1: Int, arg2: Long): Long = _f_cast.apply(arg1, arg2)
          }
        case 56 =>
          val _f_cast = _f.asInstanceOf[Function2[Long, Int, Unit]]
          new MiniboxedFunction2[Long, Int, Unit] {
            def extractFunctionX: Function2[Long, Int, Unit] = _f_cast
            def apply(arg1: Long, arg2: Int): Unit = _f_cast.apply(arg1, arg2)
          }
        case 156 =>
          val _f_cast = _f.asInstanceOf[Function2[Long, Int, Boolean]]
          new MiniboxedFunction2[Long, Int, Boolean] {
            def extractFunctionX: Function2[Long, Int, Boolean] = _f_cast
            def apply(arg1: Long, arg2: Int): Boolean = _f_cast.apply(arg1, arg2)
          }
        case 556 =>
          val _f_cast = _f.asInstanceOf[Function2[Long, Int, Int]]
          new MiniboxedFunction2[Long, Int, Int] {
            def extractFunctionX: Function2[Long, Int, Int] = _f_cast
            def apply(arg1: Long, arg2: Int): Int = _f_cast.apply(arg1, arg2)
          }
        case 656 =>
          val _f_cast = _f.asInstanceOf[Function2[Long, Int, Long]]
          new MiniboxedFunction2[Long, Int, Long] {
            def extractFunctionX: Function2[Long, Int, Long] = _f_cast
            def apply(arg1: Long, arg2: Int): Long = _f_cast.apply(arg1, arg2)
          }
        case 66 =>
          val _f_cast = _f.asInstanceOf[Function2[Long, Long, Unit]]
          new MiniboxedFunction2[Long, Long, Unit] {
            def extractFunctionX: Function2[Long, Long, Unit] = _f_cast
            def apply(arg1: Long, arg2: Long): Unit = _f_cast.apply(arg1, arg2)
          }
        case 166 =>
          val _f_cast = _f.asInstanceOf[Function2[Long, Long, Boolean]]
          new MiniboxedFunction2[Long, Long, Boolean] {
            def extractFunctionX: Function2[Long, Long, Boolean] = _f_cast
            def apply(arg1: Long, arg2: Long): Boolean = _f_cast.apply(arg1, arg2)
          }
        case 566 =>
          val _f_cast = _f.asInstanceOf[Function2[Long, Long, Int]]
          new MiniboxedFunction2[Long, Long, Int] {
            def extractFunctionX: Function2[Long, Long, Int] = _f_cast
            def apply(arg1: Long, arg2: Long): Int = _f_cast.apply(arg1, arg2)
          }
        case 666 =>
          val _f_cast = _f.asInstanceOf[Function2[Long, Long, Long]]
          new MiniboxedFunction2[Long, Long, Long] {
            def extractFunctionX: Function2[Long, Long, Long] = _f_cast
            def apply(arg1: Long, arg2: Long): Long = _f_cast.apply(arg1, arg2)
          }
        case _ =>
          function2_bridge(_f)
      }).asInstanceOf[MiniboxedFunction2[T1, T2, R]]


  def function2_opt_bridge_long_double_long[T1, T2, R](T1_Tag: Byte, T2_Tag: Byte, R_Tag: Byte, _f: Function2[T1, T2, R]): MiniboxedFunction2[T1, T2, R] =
    if (_f.isInstanceOf[AbstractFunction2Wrapper[_, _, _]])
      _f.asInstanceOf[AbstractFunction2Wrapper[_, _, _]].m.asInstanceOf[MiniboxedFunction2[T1, T2, R]]
    else
      ((T1_Tag + R_Tag * 10) match {
        case 5 =>
          val _f_cast = _f.asInstanceOf[Function2[Int, Double, Unit]]
          new MiniboxedFunction2[Int, Double, Unit] {
            def extractFunctionX: Function2[Int, Double, Unit] = _f_cast
            def apply(arg1: Int, arg2: Double): Unit = _f_cast.apply(arg1, arg2)
          }
        case 15 =>
          val _f_cast = _f.asInstanceOf[Function2[Int, Double, Boolean]]
          new MiniboxedFunction2[Int, Double, Boolean] {
            def extractFunctionX: Function2[Int, Double, Boolean] = _f_cast
            def apply(arg1: Int, arg2: Double): Boolean = _f_cast.apply(arg1, arg2)
          }
        case 55 =>
          val _f_cast = _f.asInstanceOf[Function2[Int, Double, Int]]
          new MiniboxedFunction2[Int, Double, Int] {
            def extractFunctionX: Function2[Int, Double, Int] = _f_cast
            def apply(arg1: Int, arg2: Double): Int = _f_cast.apply(arg1, arg2)
          }
        case 65 =>
          val _f_cast = _f.asInstanceOf[Function2[Int, Double, Long]]
          new MiniboxedFunction2[Int, Double, Long] {
            def extractFunctionX: Function2[Int, Double, Long] = _f_cast
            def apply(arg1: Int, arg2: Double): Long = _f_cast.apply(arg1, arg2)
          }
        case 6 =>
          val _f_cast = _f.asInstanceOf[Function2[Long, Double, Unit]]
          new MiniboxedFunction2[Long, Double, Unit] {
            def extractFunctionX: Function2[Long, Double, Unit] = _f_cast
            def apply(arg1: Long, arg2: Double): Unit = _f_cast.apply(arg1, arg2)
          }
        case 16 =>
          val _f_cast = _f.asInstanceOf[Function2[Long, Double, Boolean]]
          new MiniboxedFunction2[Long, Double, Boolean] {
            def extractFunctionX: Function2[Long, Double, Boolean] = _f_cast
            def apply(arg1: Long, arg2: Double): Boolean = _f_cast.apply(arg1, arg2)
          }
        case 56 =>
          val _f_cast = _f.asInstanceOf[Function2[Long, Double, Int]]
          new MiniboxedFunction2[Long, Double, Int] {
            def extractFunctionX: Function2[Long, Double, Int] = _f_cast
            def apply(arg1: Long, arg2: Double): Int = _f_cast.apply(arg1, arg2)
          }
        case 66 =>
          val _f_cast = _f.asInstanceOf[Function2[Long, Double, Long]]
          new MiniboxedFunction2[Long, Double, Long] {
            def extractFunctionX: Function2[Long, Double, Long] = _f_cast
            def apply(arg1: Long, arg2: Double): Long = _f_cast.apply(arg1, arg2)
          }
        case _ =>
          function2_bridge(_f)
      }).asInstanceOf[MiniboxedFunction2[T1, T2, R]]


  def function2_opt_bridge_double_long_long[T1, T2, R](T1_Tag: Byte, T2_Tag: Byte, R_Tag: Byte, _f: Function2[T1, T2, R]): MiniboxedFunction2[T1, T2, R] =
    if (_f.isInstanceOf[AbstractFunction2Wrapper[_, _, _]])
      _f.asInstanceOf[AbstractFunction2Wrapper[_, _, _]].m.asInstanceOf[MiniboxedFunction2[T1, T2, R]]
    else
      ((T2_Tag + R_Tag * 10) match {
        case 5 =>
          val _f_cast = _f.asInstanceOf[Function2[Double, Int, Unit]]
          new MiniboxedFunction2[Double, Int, Unit] {
            def extractFunctionX: Function2[Double, Int, Unit] = _f_cast
            def apply(arg1: Double, arg2: Int): Unit = _f_cast.apply(arg1, arg2)
          }
        case 15 =>
          val _f_cast = _f.asInstanceOf[Function2[Double, Int, Boolean]]
          new MiniboxedFunction2[Double, Int, Boolean] {
            def extractFunctionX: Function2[Double, Int, Boolean] = _f_cast
            def apply(arg1: Double, arg2: Int): Boolean = _f_cast.apply(arg1, arg2)
          }
        case 55 =>
          val _f_cast = _f.asInstanceOf[Function2[Double, Int, Int]]
          new MiniboxedFunction2[Double, Int, Int] {
            def extractFunctionX: Function2[Double, Int, Int] = _f_cast
            def apply(arg1: Double, arg2: Int): Int = _f_cast.apply(arg1, arg2)
          }
        case 65 =>
          val _f_cast = _f.asInstanceOf[Function2[Double, Int, Long]]
          new MiniboxedFunction2[Double, Int, Long] {
            def extractFunctionX: Function2[Double, Int, Long] = _f_cast
            def apply(arg1: Double, arg2: Int): Long = _f_cast.apply(arg1, arg2)
          }
        case 6 =>
          val _f_cast = _f.asInstanceOf[Function2[Double, Long, Unit]]
          new MiniboxedFunction2[Double, Long, Unit] {
            def extractFunctionX: Function2[Double, Long, Unit] = _f_cast
            def apply(arg1: Double, arg2: Long): Unit = _f_cast.apply(arg1, arg2)
          }
        case 16 =>
          val _f_cast = _f.asInstanceOf[Function2[Double, Long, Boolean]]
          new MiniboxedFunction2[Double, Long, Boolean] {
            def extractFunctionX: Function2[Double, Long, Boolean] = _f_cast
            def apply(arg1: Double, arg2: Long): Boolean = _f_cast.apply(arg1, arg2)
          }
        case 56 =>
          val _f_cast = _f.asInstanceOf[Function2[Double, Long, Int]]
          new MiniboxedFunction2[Double, Long, Int] {
            def extractFunctionX: Function2[Double, Long, Int] = _f_cast
            def apply(arg1: Double, arg2: Long): Int = _f_cast.apply(arg1, arg2)
          }
        case 66 =>
          val _f_cast = _f.asInstanceOf[Function2[Double, Long, Long]]
          new MiniboxedFunction2[Double, Long, Long] {
            def extractFunctionX: Function2[Double, Long, Long] = _f_cast
            def apply(arg1: Double, arg2: Long): Long = _f_cast.apply(arg1, arg2)
          }
        case _ =>
          function2_bridge(_f)
      }).asInstanceOf[MiniboxedFunction2[T1, T2, R]]


  def function2_opt_bridge_double_double_long[T1, T2, R](T1_Tag: Byte, T2_Tag: Byte, R_Tag: Byte, _f: Function2[T1, T2, R]): MiniboxedFunction2[T1, T2, R] =
    if (_f.isInstanceOf[AbstractFunction2Wrapper[_, _, _]])
      _f.asInstanceOf[AbstractFunction2Wrapper[_, _, _]].m.asInstanceOf[MiniboxedFunction2[T1, T2, R]]
    else
      ((R_Tag) match {
        case 0 =>
          val _f_cast = _f.asInstanceOf[Function2[Double, Double, Unit]]
          new MiniboxedFunction2[Double, Double, Unit] {
            def extractFunctionX: Function2[Double, Double, Unit] = _f_cast
            def apply(arg1: Double, arg2: Double): Unit = _f_cast.apply(arg1, arg2)
          }
        case 1 =>
          val _f_cast = _f.asInstanceOf[Function2[Double, Double, Boolean]]
          new MiniboxedFunction2[Double, Double, Boolean] {
            def extractFunctionX: Function2[Double, Double, Boolean] = _f_cast
            def apply(arg1: Double, arg2: Double): Boolean = _f_cast.apply(arg1, arg2)
          }
        case 5 =>
          val _f_cast = _f.asInstanceOf[Function2[Double, Double, Int]]
          new MiniboxedFunction2[Double, Double, Int] {
            def extractFunctionX: Function2[Double, Double, Int] = _f_cast
            def apply(arg1: Double, arg2: Double): Int = _f_cast.apply(arg1, arg2)
          }
        case 6 =>
          val _f_cast = _f.asInstanceOf[Function2[Double, Double, Long]]
          new MiniboxedFunction2[Double, Double, Long] {
            def extractFunctionX: Function2[Double, Double, Long] = _f_cast
            def apply(arg1: Double, arg2: Double): Long = _f_cast.apply(arg1, arg2)
          }
        case _ =>
          function2_bridge(_f)
      }).asInstanceOf[MiniboxedFunction2[T1, T2, R]]


  def function2_opt_bridge_long_long_double[T1, T2, R](T1_Tag: Byte, T2_Tag: Byte, R_Tag: Byte, _f: Function2[T1, T2, R]): MiniboxedFunction2[T1, T2, R] =
    if (_f.isInstanceOf[AbstractFunction2Wrapper[_, _, _]])
      _f.asInstanceOf[AbstractFunction2Wrapper[_, _, _]].m.asInstanceOf[MiniboxedFunction2[T1, T2, R]]
    else
      ((T1_Tag + T2_Tag * 10 + R_Tag * 10 * 10) match {
        case 755 =>
          val _f_cast = _f.asInstanceOf[Function2[Int, Int, Float]]
          new MiniboxedFunction2[Int, Int, Float] {
            def extractFunctionX: Function2[Int, Int, Float] = _f_cast
            def apply(arg1: Int, arg2: Int): Float = _f_cast.apply(arg1, arg2)
          }
        case 855 =>
          val _f_cast = _f.asInstanceOf[Function2[Int, Int, Double]]
          new MiniboxedFunction2[Int, Int, Double] {
            def extractFunctionX: Function2[Int, Int, Double] = _f_cast
            def apply(arg1: Int, arg2: Int): Double = _f_cast.apply(arg1, arg2)
          }
        case 765 =>
          val _f_cast = _f.asInstanceOf[Function2[Int, Long, Float]]
          new MiniboxedFunction2[Int, Long, Float] {
            def extractFunctionX: Function2[Int, Long, Float] = _f_cast
            def apply(arg1: Int, arg2: Long): Float = _f_cast.apply(arg1, arg2)
          }
        case 865 =>
          val _f_cast = _f.asInstanceOf[Function2[Int, Long, Double]]
          new MiniboxedFunction2[Int, Long, Double] {
            def extractFunctionX: Function2[Int, Long, Double] = _f_cast
            def apply(arg1: Int, arg2: Long): Double = _f_cast.apply(arg1, arg2)
          }
        case 756 =>
          val _f_cast = _f.asInstanceOf[Function2[Long, Int, Float]]
          new MiniboxedFunction2[Long, Int, Float] {
            def extractFunctionX: Function2[Long, Int, Float] = _f_cast
            def apply(arg1: Long, arg2: Int): Float = _f_cast.apply(arg1, arg2)
          }
        case 856 =>
          val _f_cast = _f.asInstanceOf[Function2[Long, Int, Double]]
          new MiniboxedFunction2[Long, Int, Double] {
            def extractFunctionX: Function2[Long, Int, Double] = _f_cast
            def apply(arg1: Long, arg2: Int): Double = _f_cast.apply(arg1, arg2)
          }
        case 766 =>
          val _f_cast = _f.asInstanceOf[Function2[Long, Long, Float]]
          new MiniboxedFunction2[Long, Long, Float] {
            def extractFunctionX: Function2[Long, Long, Float] = _f_cast
            def apply(arg1: Long, arg2: Long): Float = _f_cast.apply(arg1, arg2)
          }
        case 866 =>
          val _f_cast = _f.asInstanceOf[Function2[Long, Long, Double]]
          new MiniboxedFunction2[Long, Long, Double] {
            def extractFunctionX: Function2[Long, Long, Double] = _f_cast
            def apply(arg1: Long, arg2: Long): Double = _f_cast.apply(arg1, arg2)
          }
        case _ =>
          function2_bridge(_f)
      }).asInstanceOf[MiniboxedFunction2[T1, T2, R]]


  def function2_opt_bridge_long_double_double[T1, T2, R](T1_Tag: Byte, T2_Tag: Byte, R_Tag: Byte, _f: Function2[T1, T2, R]): MiniboxedFunction2[T1, T2, R] =
    if (_f.isInstanceOf[AbstractFunction2Wrapper[_, _, _]])
      _f.asInstanceOf[AbstractFunction2Wrapper[_, _, _]].m.asInstanceOf[MiniboxedFunction2[T1, T2, R]]
    else
      ((T1_Tag + R_Tag * 10) match {
        case 75 =>
          val _f_cast = _f.asInstanceOf[Function2[Int, Double, Float]]
          new MiniboxedFunction2[Int, Double, Float] {
            def extractFunctionX: Function2[Int, Double, Float] = _f_cast
            def apply(arg1: Int, arg2: Double): Float = _f_cast.apply(arg1, arg2)
          }
        case 85 =>
          val _f_cast = _f.asInstanceOf[Function2[Int, Double, Double]]
          new MiniboxedFunction2[Int, Double, Double] {
            def extractFunctionX: Function2[Int, Double, Double] = _f_cast
            def apply(arg1: Int, arg2: Double): Double = _f_cast.apply(arg1, arg2)
          }
        case 76 =>
          val _f_cast = _f.asInstanceOf[Function2[Long, Double, Float]]
          new MiniboxedFunction2[Long, Double, Float] {
            def extractFunctionX: Function2[Long, Double, Float] = _f_cast
            def apply(arg1: Long, arg2: Double): Float = _f_cast.apply(arg1, arg2)
          }
        case 86 =>
          val _f_cast = _f.asInstanceOf[Function2[Long, Double, Double]]
          new MiniboxedFunction2[Long, Double, Double] {
            def extractFunctionX: Function2[Long, Double, Double] = _f_cast
            def apply(arg1: Long, arg2: Double): Double = _f_cast.apply(arg1, arg2)
          }
        case _ =>
          function2_bridge(_f)
      }).asInstanceOf[MiniboxedFunction2[T1, T2, R]]


  def function2_opt_bridge_double_long_double[T1, T2, R](T1_Tag: Byte, T2_Tag: Byte, R_Tag: Byte, _f: Function2[T1, T2, R]): MiniboxedFunction2[T1, T2, R] =
    if (_f.isInstanceOf[AbstractFunction2Wrapper[_, _, _]])
      _f.asInstanceOf[AbstractFunction2Wrapper[_, _, _]].m.asInstanceOf[MiniboxedFunction2[T1, T2, R]]
    else
      ((T2_Tag + R_Tag * 10) match {
        case 75 =>
          val _f_cast = _f.asInstanceOf[Function2[Double, Int, Float]]
          new MiniboxedFunction2[Double, Int, Float] {
            def extractFunctionX: Function2[Double, Int, Float] = _f_cast
            def apply(arg1: Double, arg2: Int): Float = _f_cast.apply(arg1, arg2)
          }
        case 85 =>
          val _f_cast = _f.asInstanceOf[Function2[Double, Int, Double]]
          new MiniboxedFunction2[Double, Int, Double] {
            def extractFunctionX: Function2[Double, Int, Double] = _f_cast
            def apply(arg1: Double, arg2: Int): Double = _f_cast.apply(arg1, arg2)
          }
        case 76 =>
          val _f_cast = _f.asInstanceOf[Function2[Double, Long, Float]]
          new MiniboxedFunction2[Double, Long, Float] {
            def extractFunctionX: Function2[Double, Long, Float] = _f_cast
            def apply(arg1: Double, arg2: Long): Float = _f_cast.apply(arg1, arg2)
          }
        case 86 =>
          val _f_cast = _f.asInstanceOf[Function2[Double, Long, Double]]
          new MiniboxedFunction2[Double, Long, Double] {
            def extractFunctionX: Function2[Double, Long, Double] = _f_cast
            def apply(arg1: Double, arg2: Long): Double = _f_cast.apply(arg1, arg2)
          }
        case _ =>
          function2_bridge(_f)
      }).asInstanceOf[MiniboxedFunction2[T1, T2, R]]


  def function2_opt_bridge_double_double_double[T1, T2, R](T1_Tag: Byte, T2_Tag: Byte, R_Tag: Byte, _f: Function2[T1, T2, R]): MiniboxedFunction2[T1, T2, R] =
    if (_f.isInstanceOf[AbstractFunction2Wrapper[_, _, _]])
      _f.asInstanceOf[AbstractFunction2Wrapper[_, _, _]].m.asInstanceOf[MiniboxedFunction2[T1, T2, R]]
    else
      ((R_Tag) match {
        case 7 =>
          val _f_cast = _f.asInstanceOf[Function2[Double, Double, Float]]
          new MiniboxedFunction2[Double, Double, Float] {
            def extractFunctionX: Function2[Double, Double, Float] = _f_cast
            def apply(arg1: Double, arg2: Double): Float = _f_cast.apply(arg1, arg2)
          }
        case 8 =>
          val _f_cast = _f.asInstanceOf[Function2[Double, Double, Double]]
          new MiniboxedFunction2[Double, Double, Double] {
            def extractFunctionX: Function2[Double, Double, Double] = _f_cast
            def apply(arg1: Double, arg2: Double): Double = _f_cast.apply(arg1, arg2)
          }
        case _ =>
          function2_bridge(_f)
      }).asInstanceOf[MiniboxedFunction2[T1, T2, R]]
}