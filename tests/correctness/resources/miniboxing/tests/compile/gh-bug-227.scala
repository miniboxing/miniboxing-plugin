object Test {
	def foo[T: Numeric] = implicitly[miniboxing.runtime.math.MiniboxedNumeric[T]]
	def foo[T: Fractional] = implicitly[miniboxing.runtime.math.MiniboxedFractional[T]]
	def foo[T: Integral] = implicitly[miniboxing.runtime.math.MiniboxedIntegral[T]]
}