package miniboxing.tests.compile.bug61

class C[@miniboxed T] {
  def foo(t: T): T = ???
}

class D extends C[Int] {
  override def foo(t: Int): Int = t + t // * t - t + 3
}

