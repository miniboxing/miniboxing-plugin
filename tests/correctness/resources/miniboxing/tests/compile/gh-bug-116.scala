package miniboxing.tests.compile.bug116

object Test extends App {
  val f = (x: Int) => { ???; x }
  try {
    f(3)
  } catch {
    case t: Throwable =>
      println(t.getStackTrace()(1).getMethodName())
  }
}
