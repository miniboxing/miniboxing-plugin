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

  // Specialized bridges:
  def function0_opt_bridge_long[R](R_Tag: Byte, _f: Function0[R]): MiniboxedFunction0[R] =
    ((R_Tag) match {
      case 5 =>
        val _f_cast = _f.asInstanceOf[Function0[Int]]
        new MiniboxedFunction0[Int] {
           def f: Function0[Int] = _f_cast
          def apply(): Int = _f_cast.apply()
        }
      case 6 =>
        val _f_cast = _f.asInstanceOf[Function0[Long]]
        new MiniboxedFunction0[Long] {
           def f: Function0[Long] = _f_cast
          def apply(): Long = _f_cast.apply()
        }
      case 4 =>
        val _f_cast = _f.asInstanceOf[Function0[Char]]
        new MiniboxedFunction0[Char] {
           def f: Function0[Char] = _f_cast
          def apply(): Char = _f_cast.apply()
        }
      case _ =>
        function0_bridge(_f)
    }).asInstanceOf[MiniboxedFunction0[R]]


  def function0_opt_bridge_double[R](R_Tag: Byte, _f: Function0[R]): MiniboxedFunction0[R] =
    ((R_Tag) match {
      case 7 =>
        val _f_cast = _f.asInstanceOf[Function0[Float]]
        new MiniboxedFunction0[Float] {
           def f: Function0[Float] = _f_cast
          def apply(): Float = _f_cast.apply()
        }
      case 8 =>
        val _f_cast = _f.asInstanceOf[Function0[Double]]
        new MiniboxedFunction0[Double] {
           def f: Function0[Double] = _f_cast
          def apply(): Double = _f_cast.apply()
        }
      case _ =>
        function0_bridge(_f)
    }).asInstanceOf[MiniboxedFunction0[R]]


  def function1_opt_bridge_long_long[T, R](T_Tag: Byte, R_Tag: Byte, _f: Function1[T, R]): MiniboxedFunction1[T, R] =
    ((T_Tag + R_Tag * 10) match {
      case 55 =>
        val _f_cast = _f.asInstanceOf[Function1[Int, Int]]
        new MiniboxedFunction1[Int, Int] {
           def f: Function1[Int, Int] = _f_cast
          def apply(arg1: Int): Int = _f_cast.apply(arg1)
        }
      case 65 =>
        val _f_cast = _f.asInstanceOf[Function1[Int, Long]]
        new MiniboxedFunction1[Int, Long] {
           def f: Function1[Int, Long] = _f_cast
          def apply(arg1: Int): Long = _f_cast.apply(arg1)
        }
      case 56 =>
        val _f_cast = _f.asInstanceOf[Function1[Long, Int]]
        new MiniboxedFunction1[Long, Int] {
           def f: Function1[Long, Int] = _f_cast
          def apply(arg1: Long): Int = _f_cast.apply(arg1)
        }
      case 66 =>
        val _f_cast = _f.asInstanceOf[Function1[Long, Long]]
        new MiniboxedFunction1[Long, Long] {
           def f: Function1[Long, Long] = _f_cast
          def apply(arg1: Long): Long = _f_cast.apply(arg1)
        }
      case _ =>
        function1_bridge(_f)
    }).asInstanceOf[MiniboxedFunction1[T, R]]


  def function1_opt_bridge_double_long[T, R](T_Tag: Byte, R_Tag: Byte, _f: Function1[T, R]): MiniboxedFunction1[T, R] =
    ((T_Tag + R_Tag * 10) match {
      case 57 =>
        val _f_cast = _f.asInstanceOf[Function1[Float, Int]]
        new MiniboxedFunction1[Float, Int] {
           def f: Function1[Float, Int] = _f_cast
          def apply(arg1: Float): Int = _f_cast.apply(arg1)
        }
      case 67 =>
        val _f_cast = _f.asInstanceOf[Function1[Float, Long]]
        new MiniboxedFunction1[Float, Long] {
           def f: Function1[Float, Long] = _f_cast
          def apply(arg1: Float): Long = _f_cast.apply(arg1)
        }
      case 58 =>
        val _f_cast = _f.asInstanceOf[Function1[Double, Int]]
        new MiniboxedFunction1[Double, Int] {
           def f: Function1[Double, Int] = _f_cast
          def apply(arg1: Double): Int = _f_cast.apply(arg1)
        }
      case 68 =>
        val _f_cast = _f.asInstanceOf[Function1[Double, Long]]
        new MiniboxedFunction1[Double, Long] {
           def f: Function1[Double, Long] = _f_cast
          def apply(arg1: Double): Long = _f_cast.apply(arg1)
        }
      case _ =>
        function1_bridge(_f)
    }).asInstanceOf[MiniboxedFunction1[T, R]]


  def function1_opt_bridge_long_double[T, R](T_Tag: Byte, R_Tag: Byte, _f: Function1[T, R]): MiniboxedFunction1[T, R] =
    ((T_Tag + R_Tag * 10) match {
      case 75 =>
        val _f_cast = _f.asInstanceOf[Function1[Int, Float]]
        new MiniboxedFunction1[Int, Float] {
           def f: Function1[Int, Float] = _f_cast
          def apply(arg1: Int): Float = _f_cast.apply(arg1)
        }
      case 85 =>
        val _f_cast = _f.asInstanceOf[Function1[Int, Double]]
        new MiniboxedFunction1[Int, Double] {
           def f: Function1[Int, Double] = _f_cast
          def apply(arg1: Int): Double = _f_cast.apply(arg1)
        }
      case 76 =>
        val _f_cast = _f.asInstanceOf[Function1[Long, Float]]
        new MiniboxedFunction1[Long, Float] {
           def f: Function1[Long, Float] = _f_cast
          def apply(arg1: Long): Float = _f_cast.apply(arg1)
        }
      case 86 =>
        val _f_cast = _f.asInstanceOf[Function1[Long, Double]]
        new MiniboxedFunction1[Long, Double] {
           def f: Function1[Long, Double] = _f_cast
          def apply(arg1: Long): Double = _f_cast.apply(arg1)
        }
      case _ =>
        function1_bridge(_f)
    }).asInstanceOf[MiniboxedFunction1[T, R]]


  def function1_opt_bridge_double_double[T, R](T_Tag: Byte, R_Tag: Byte, _f: Function1[T, R]): MiniboxedFunction1[T, R] =
    ((T_Tag + R_Tag * 10) match {
      case 77 =>
        val _f_cast = _f.asInstanceOf[Function1[Float, Float]]
        new MiniboxedFunction1[Float, Float] {
           def f: Function1[Float, Float] = _f_cast
          def apply(arg1: Float): Float = _f_cast.apply(arg1)
        }
      case 87 =>
        val _f_cast = _f.asInstanceOf[Function1[Float, Double]]
        new MiniboxedFunction1[Float, Double] {
           def f: Function1[Float, Double] = _f_cast
          def apply(arg1: Float): Double = _f_cast.apply(arg1)
        }
      case 78 =>
        val _f_cast = _f.asInstanceOf[Function1[Double, Float]]
        new MiniboxedFunction1[Double, Float] {
           def f: Function1[Double, Float] = _f_cast
          def apply(arg1: Double): Float = _f_cast.apply(arg1)
        }
      case 88 =>
        val _f_cast = _f.asInstanceOf[Function1[Double, Double]]
        new MiniboxedFunction1[Double, Double] {
           def f: Function1[Double, Double] = _f_cast
          def apply(arg1: Double): Double = _f_cast.apply(arg1)
        }
      case _ =>
        function1_bridge(_f)
    }).asInstanceOf[MiniboxedFunction1[T, R]]


  def function2_opt_bridge_long_long_long[T1, T2, R](T1_Tag: Byte, T2_Tag: Byte, R_Tag: Byte, _f: Function2[T1, T2, R]): MiniboxedFunction2[T1, T2, R] =
    ((T1_Tag + T2_Tag * 10 + R_Tag * 10 * 10) match {
      case 555 =>
        val _f_cast = _f.asInstanceOf[Function2[Int, Int, Int]]
        new MiniboxedFunction2[Int, Int, Int] {
           def f: Function2[Int, Int, Int] = _f_cast
          def apply(arg1: Int, arg2: Int): Int = _f_cast.apply(arg1, arg2)
        }
      case 655 =>
        val _f_cast = _f.asInstanceOf[Function2[Int, Int, Long]]
        new MiniboxedFunction2[Int, Int, Long] {
           def f: Function2[Int, Int, Long] = _f_cast
          def apply(arg1: Int, arg2: Int): Long = _f_cast.apply(arg1, arg2)
        }
      case 565 =>
        val _f_cast = _f.asInstanceOf[Function2[Int, Long, Int]]
        new MiniboxedFunction2[Int, Long, Int] {
           def f: Function2[Int, Long, Int] = _f_cast
          def apply(arg1: Int, arg2: Long): Int = _f_cast.apply(arg1, arg2)
        }
      case 665 =>
        val _f_cast = _f.asInstanceOf[Function2[Int, Long, Long]]
        new MiniboxedFunction2[Int, Long, Long] {
           def f: Function2[Int, Long, Long] = _f_cast
          def apply(arg1: Int, arg2: Long): Long = _f_cast.apply(arg1, arg2)
        }
      case 556 =>
        val _f_cast = _f.asInstanceOf[Function2[Long, Int, Int]]
        new MiniboxedFunction2[Long, Int, Int] {
           def f: Function2[Long, Int, Int] = _f_cast
          def apply(arg1: Long, arg2: Int): Int = _f_cast.apply(arg1, arg2)
        }
      case 656 =>
        val _f_cast = _f.asInstanceOf[Function2[Long, Int, Long]]
        new MiniboxedFunction2[Long, Int, Long] {
           def f: Function2[Long, Int, Long] = _f_cast
          def apply(arg1: Long, arg2: Int): Long = _f_cast.apply(arg1, arg2)
        }
      case 566 =>
        val _f_cast = _f.asInstanceOf[Function2[Long, Long, Int]]
        new MiniboxedFunction2[Long, Long, Int] {
           def f: Function2[Long, Long, Int] = _f_cast
          def apply(arg1: Long, arg2: Long): Int = _f_cast.apply(arg1, arg2)
        }
      case 666 =>
        val _f_cast = _f.asInstanceOf[Function2[Long, Long, Long]]
        new MiniboxedFunction2[Long, Long, Long] {
           def f: Function2[Long, Long, Long] = _f_cast
          def apply(arg1: Long, arg2: Long): Long = _f_cast.apply(arg1, arg2)
        }
      case _ =>
        function2_bridge(_f)
    }).asInstanceOf[MiniboxedFunction2[T1, T2, R]]


  def function2_opt_bridge_long_double_long[T1, T2, R](T1_Tag: Byte, T2_Tag: Byte, R_Tag: Byte, _f: Function2[T1, T2, R]): MiniboxedFunction2[T1, T2, R] =
    ((T1_Tag + R_Tag * 10) match {
      case 55 =>
        val _f_cast = _f.asInstanceOf[Function2[Int, Double, Int]]
        new MiniboxedFunction2[Int, Double, Int] {
           def f: Function2[Int, Double, Int] = _f_cast
          def apply(arg1: Int, arg2: Double): Int = _f_cast.apply(arg1, arg2)
        }
      case 65 =>
        val _f_cast = _f.asInstanceOf[Function2[Int, Double, Long]]
        new MiniboxedFunction2[Int, Double, Long] {
           def f: Function2[Int, Double, Long] = _f_cast
          def apply(arg1: Int, arg2: Double): Long = _f_cast.apply(arg1, arg2)
        }
      case 56 =>
        val _f_cast = _f.asInstanceOf[Function2[Long, Double, Int]]
        new MiniboxedFunction2[Long, Double, Int] {
           def f: Function2[Long, Double, Int] = _f_cast
          def apply(arg1: Long, arg2: Double): Int = _f_cast.apply(arg1, arg2)
        }
      case 66 =>
        val _f_cast = _f.asInstanceOf[Function2[Long, Double, Long]]
        new MiniboxedFunction2[Long, Double, Long] {
           def f: Function2[Long, Double, Long] = _f_cast
          def apply(arg1: Long, arg2: Double): Long = _f_cast.apply(arg1, arg2)
        }
      case _ =>
        function2_bridge(_f)
    }).asInstanceOf[MiniboxedFunction2[T1, T2, R]]


  def function2_opt_bridge_double_long_long[T1, T2, R](T1_Tag: Byte, T2_Tag: Byte, R_Tag: Byte, _f: Function2[T1, T2, R]): MiniboxedFunction2[T1, T2, R] =
    ((T2_Tag + R_Tag * 10) match {
      case 55 =>
        val _f_cast = _f.asInstanceOf[Function2[Double, Int, Int]]
        new MiniboxedFunction2[Double, Int, Int] {
           def f: Function2[Double, Int, Int] = _f_cast
          def apply(arg1: Double, arg2: Int): Int = _f_cast.apply(arg1, arg2)
        }
      case 65 =>
        val _f_cast = _f.asInstanceOf[Function2[Double, Int, Long]]
        new MiniboxedFunction2[Double, Int, Long] {
           def f: Function2[Double, Int, Long] = _f_cast
          def apply(arg1: Double, arg2: Int): Long = _f_cast.apply(arg1, arg2)
        }
      case 56 =>
        val _f_cast = _f.asInstanceOf[Function2[Double, Long, Int]]
        new MiniboxedFunction2[Double, Long, Int] {
           def f: Function2[Double, Long, Int] = _f_cast
          def apply(arg1: Double, arg2: Long): Int = _f_cast.apply(arg1, arg2)
        }
      case 66 =>
        val _f_cast = _f.asInstanceOf[Function2[Double, Long, Long]]
        new MiniboxedFunction2[Double, Long, Long] {
           def f: Function2[Double, Long, Long] = _f_cast
          def apply(arg1: Double, arg2: Long): Long = _f_cast.apply(arg1, arg2)
        }
      case _ =>
        function2_bridge(_f)
    }).asInstanceOf[MiniboxedFunction2[T1, T2, R]]


  def function2_opt_bridge_double_double_long[T1, T2, R](T1_Tag: Byte, T2_Tag: Byte, R_Tag: Byte, _f: Function2[T1, T2, R]): MiniboxedFunction2[T1, T2, R] =
    ((R_Tag) match {
      case 5 =>
        val _f_cast = _f.asInstanceOf[Function2[Double, Double, Int]]
        new MiniboxedFunction2[Double, Double, Int] {
           def f: Function2[Double, Double, Int] = _f_cast
          def apply(arg1: Double, arg2: Double): Int = _f_cast.apply(arg1, arg2)
        }
      case 6 =>
        val _f_cast = _f.asInstanceOf[Function2[Double, Double, Long]]
        new MiniboxedFunction2[Double, Double, Long] {
           def f: Function2[Double, Double, Long] = _f_cast
          def apply(arg1: Double, arg2: Double): Long = _f_cast.apply(arg1, arg2)
        }
      case _ =>
        function2_bridge(_f)
    }).asInstanceOf[MiniboxedFunction2[T1, T2, R]]


  def function2_opt_bridge_long_long_double[T1, T2, R](T1_Tag: Byte, T2_Tag: Byte, R_Tag: Byte, _f: Function2[T1, T2, R]): MiniboxedFunction2[T1, T2, R] =
    ((T1_Tag + T2_Tag * 10 + R_Tag * 10 * 10) match {
      case 755 =>
        val _f_cast = _f.asInstanceOf[Function2[Int, Int, Float]]
        new MiniboxedFunction2[Int, Int, Float] {
           def f: Function2[Int, Int, Float] = _f_cast
          def apply(arg1: Int, arg2: Int): Float = _f_cast.apply(arg1, arg2)
        }
      case 855 =>
        val _f_cast = _f.asInstanceOf[Function2[Int, Int, Double]]
        new MiniboxedFunction2[Int, Int, Double] {
           def f: Function2[Int, Int, Double] = _f_cast
          def apply(arg1: Int, arg2: Int): Double = _f_cast.apply(arg1, arg2)
        }
      case 765 =>
        val _f_cast = _f.asInstanceOf[Function2[Int, Long, Float]]
        new MiniboxedFunction2[Int, Long, Float] {
           def f: Function2[Int, Long, Float] = _f_cast
          def apply(arg1: Int, arg2: Long): Float = _f_cast.apply(arg1, arg2)
        }
      case 865 =>
        val _f_cast = _f.asInstanceOf[Function2[Int, Long, Double]]
        new MiniboxedFunction2[Int, Long, Double] {
           def f: Function2[Int, Long, Double] = _f_cast
          def apply(arg1: Int, arg2: Long): Double = _f_cast.apply(arg1, arg2)
        }
      case 756 =>
        val _f_cast = _f.asInstanceOf[Function2[Long, Int, Float]]
        new MiniboxedFunction2[Long, Int, Float] {
           def f: Function2[Long, Int, Float] = _f_cast
          def apply(arg1: Long, arg2: Int): Float = _f_cast.apply(arg1, arg2)
        }
      case 856 =>
        val _f_cast = _f.asInstanceOf[Function2[Long, Int, Double]]
        new MiniboxedFunction2[Long, Int, Double] {
           def f: Function2[Long, Int, Double] = _f_cast
          def apply(arg1: Long, arg2: Int): Double = _f_cast.apply(arg1, arg2)
        }
      case 766 =>
        val _f_cast = _f.asInstanceOf[Function2[Long, Long, Float]]
        new MiniboxedFunction2[Long, Long, Float] {
           def f: Function2[Long, Long, Float] = _f_cast
          def apply(arg1: Long, arg2: Long): Float = _f_cast.apply(arg1, arg2)
        }
      case 866 =>
        val _f_cast = _f.asInstanceOf[Function2[Long, Long, Double]]
        new MiniboxedFunction2[Long, Long, Double] {
           def f: Function2[Long, Long, Double] = _f_cast
          def apply(arg1: Long, arg2: Long): Double = _f_cast.apply(arg1, arg2)
        }
      case _ =>
        function2_bridge(_f)
    }).asInstanceOf[MiniboxedFunction2[T1, T2, R]]


  def function2_opt_bridge_long_double_double[T1, T2, R](T1_Tag: Byte, T2_Tag: Byte, R_Tag: Byte, _f: Function2[T1, T2, R]): MiniboxedFunction2[T1, T2, R] =
    ((T1_Tag + R_Tag * 10) match {
      case 75 =>
        val _f_cast = _f.asInstanceOf[Function2[Int, Double, Float]]
        new MiniboxedFunction2[Int, Double, Float] {
           def f: Function2[Int, Double, Float] = _f_cast
          def apply(arg1: Int, arg2: Double): Float = _f_cast.apply(arg1, arg2)
        }
      case 85 =>
        val _f_cast = _f.asInstanceOf[Function2[Int, Double, Double]]
        new MiniboxedFunction2[Int, Double, Double] {
           def f: Function2[Int, Double, Double] = _f_cast
          def apply(arg1: Int, arg2: Double): Double = _f_cast.apply(arg1, arg2)
        }
      case 76 =>
        val _f_cast = _f.asInstanceOf[Function2[Long, Double, Float]]
        new MiniboxedFunction2[Long, Double, Float] {
           def f: Function2[Long, Double, Float] = _f_cast
          def apply(arg1: Long, arg2: Double): Float = _f_cast.apply(arg1, arg2)
        }
      case 86 =>
        val _f_cast = _f.asInstanceOf[Function2[Long, Double, Double]]
        new MiniboxedFunction2[Long, Double, Double] {
           def f: Function2[Long, Double, Double] = _f_cast
          def apply(arg1: Long, arg2: Double): Double = _f_cast.apply(arg1, arg2)
        }
      case _ =>
        function2_bridge(_f)
    }).asInstanceOf[MiniboxedFunction2[T1, T2, R]]


  def function2_opt_bridge_double_long_double[T1, T2, R](T1_Tag: Byte, T2_Tag: Byte, R_Tag: Byte, _f: Function2[T1, T2, R]): MiniboxedFunction2[T1, T2, R] =
    ((T2_Tag + R_Tag * 10) match {
      case 75 =>
        val _f_cast = _f.asInstanceOf[Function2[Double, Int, Float]]
        new MiniboxedFunction2[Double, Int, Float] {
           def f: Function2[Double, Int, Float] = _f_cast
          def apply(arg1: Double, arg2: Int): Float = _f_cast.apply(arg1, arg2)
        }
      case 85 =>
        val _f_cast = _f.asInstanceOf[Function2[Double, Int, Double]]
        new MiniboxedFunction2[Double, Int, Double] {
           def f: Function2[Double, Int, Double] = _f_cast
          def apply(arg1: Double, arg2: Int): Double = _f_cast.apply(arg1, arg2)
        }
      case 76 =>
        val _f_cast = _f.asInstanceOf[Function2[Double, Long, Float]]
        new MiniboxedFunction2[Double, Long, Float] {
           def f: Function2[Double, Long, Float] = _f_cast
          def apply(arg1: Double, arg2: Long): Float = _f_cast.apply(arg1, arg2)
        }
      case 86 =>
        val _f_cast = _f.asInstanceOf[Function2[Double, Long, Double]]
        new MiniboxedFunction2[Double, Long, Double] {
           def f: Function2[Double, Long, Double] = _f_cast
          def apply(arg1: Double, arg2: Long): Double = _f_cast.apply(arg1, arg2)
        }
      case _ =>
        function2_bridge(_f)
    }).asInstanceOf[MiniboxedFunction2[T1, T2, R]]


  def function2_opt_bridge_double_double_double[T1, T2, R](T1_Tag: Byte, T2_Tag: Byte, R_Tag: Byte, _f: Function2[T1, T2, R]): MiniboxedFunction2[T1, T2, R] =
    ((R_Tag) match {
      case 7 =>
        val _f_cast = _f.asInstanceOf[Function2[Double, Double, Float]]
        new MiniboxedFunction2[Double, Double, Float] {
           def f: Function2[Double, Double, Float] = _f_cast
          def apply(arg1: Double, arg2: Double): Float = _f_cast.apply(arg1, arg2)
        }
      case 8 =>
        val _f_cast = _f.asInstanceOf[Function2[Double, Double, Double]]
        new MiniboxedFunction2[Double, Double, Double] {
           def f: Function2[Double, Double, Double] = _f_cast
          def apply(arg1: Double, arg2: Double): Double = _f_cast.apply(arg1, arg2)
        }
      case _ =>
        function2_bridge(_f)
    }).asInstanceOf[MiniboxedFunction2[T1, T2, R]]
}