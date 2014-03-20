package miniboxing.tests.compile

class C[@miniboxed T] {
  def foo(t: T): T = t
}

class D extends C[Int] {
  override def foo(t: Int): Int = t + 1
}


class F extends C[Long] {
  override def foo(t: Long): Long = t + 1
}

class G extends C[Double] {
  override def foo(t: Double): Double = t + 1
}

class H extends C[Float] {
  override def foo(t: Float): Float = t + 1
}