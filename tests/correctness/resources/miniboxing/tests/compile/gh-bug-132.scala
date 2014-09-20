package miniboxing.tests.compile.bug132

package object test {

  // Match's companion object is private
  // but the inferencer can access it :)
  class Match(msg: String)
  private[test] object Match {
    implicit def wtf: Result[Match] = ???
  }

  // used to trigger the inferencer:
  trait Result[T]
  def hello[T: Result] = ???
}

object Test {
  def main(args: Array[String]): Unit = {
    import test._
    hello[Match] // implicitly it's hello[Match](Match.wtf)
  }
} 
