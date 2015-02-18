package miniboxing.tests.compile.bug116

object Test {

  def main(args: Array[String]): Unit = {
    val f = (x: Int) => { ???; x }
    try {
      f(3)
    } catch {
      case t: Throwable =>
        println(t.getStackTrace()(2).getMethodName())
        //t.printStackTrace()
    }
  }

}
