package miniboxing.tests.compile.bug166b

trait A[@miniboxed T] {
  def getStr: String = "aaa"
}
trait B[@miniboxed T] extends A[T] {
  override def getStr: String = super.getStr
  
  // add a direct accessor in B:
  def BsuperGetStr = B.super.getStr

  class X {
    // call it from X:
    def baz = println(BsuperGetStr)
  }
}
