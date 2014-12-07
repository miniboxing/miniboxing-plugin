package miniboxing.tests.compile.bug163

trait Test[@miniboxed A] {
  def firstdiv(x: Int): Int = {
    try {
      println("x")
    } catch {
      case ex: NullPointerException =>
        println(ex)
    }

    for (i <- 1 to 10) {
      return i
    }
    x
  }
}
