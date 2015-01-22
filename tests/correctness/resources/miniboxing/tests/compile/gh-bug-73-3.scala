package miniboxing.tests.compile.bug73.ver3

trait C1[@miniboxed +A] {
  def head: A = sys.error("")
}
trait C2[+A] extends C1[A] {
  override def head: A = super.head
}
class C3 extends C2[Char] 
