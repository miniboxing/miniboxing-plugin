package miniboxing.tests.compile.bug185

object Test {
  def main(args: Array[String]): Unit = {
    // the two problems that can occur:
    val c = new C[Int]
    object O extends C[String]
  }
}

