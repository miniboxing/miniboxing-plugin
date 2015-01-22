package miniboxing.tests.compile.bug73.ver4

trait C1[+A] {
  def head: Int = sys.error("")
}
trait C2[+A] extends C1[A] {
  override def head: Int = super.head
}
class C3 extends C2[Char] 
