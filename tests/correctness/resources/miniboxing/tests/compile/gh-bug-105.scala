package miniboxing.tests.compile.bug105

class C[@miniboxed T] {

  var array: Array[Int] = null

  init()

  def init(): Unit = {
    println("init() called.")
    array = new Array[Int](16)
  }

  def put(i: Int): Unit = {
    array(i) = i
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C[Int]  // prints "init() called."
    c.put(3)            // NullPointerException: array => fixed :)
  }
}
