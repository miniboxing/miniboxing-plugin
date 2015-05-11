package miniboxing.tests.compile.bug185

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C[Int] // unless the miniboxing plugin is attached, this is incorrect bytecode
  }
}
