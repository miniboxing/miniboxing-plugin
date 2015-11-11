object Test {
  def foo[T: Numeric] = implicitly[MiniboxedNumeric[T]]
  def foo[T: Fractional] = implicitly[MiniboxedFractional[T]]
  def foo[T: Integral] = implicitly[MiniboxedIntegral[T]]
}