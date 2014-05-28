package miniboxing.tests.compile.sup1

class C[@miniboxed T] {
  def foo(t: T): T = ???
}

class D extends C[Int] {
  override def foo(i: Int): Int = 
    super.foo(3)
}
