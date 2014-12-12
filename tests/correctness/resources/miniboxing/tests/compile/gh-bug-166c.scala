package miniboxing.tests.compile.bug166c

trait A[@miniboxed T] {
  def getStr: String = "aaa"
}
trait B[@miniboxed T] extends A[T] {
  override def getStr: String = super[A].getStr
}
