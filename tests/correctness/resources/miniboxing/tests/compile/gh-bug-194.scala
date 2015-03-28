class C[@generic @miniboxed U](t:U) {}

object Test {
  def test1[@generic T](t:T) = { }
	def test2[@miniboxed U](u: U) = {
		test1[U](u)
	}
	def test3[@generic @miniboxed S](s:S) = { }
  def test4[X](x: X) = new C[X](x)
	def main(args: Array[String]) {
		test1[Int](3)
		test2[Int](4)
		test3[Any](5)
	}
}
