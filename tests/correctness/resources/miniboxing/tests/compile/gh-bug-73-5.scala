package miniboxing.tests.compile.bug73.ver5

trait C1[@miniboxed +A, B] {
  def head: B = sys.error("")
}

trait C2[@miniboxed +A, @miniboxed B] extends C1[A, B] {
  override def head: B = super.head
}

class C3 extends C2[Int, Int]
