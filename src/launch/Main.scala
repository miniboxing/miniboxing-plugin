package launch

object Main {
  def main(args: Array[String]) {
    val args = Array("-Xplugin:lib/minibox.jar", "test/Test.scala", "-Ystop-after:cleanup")
    println("running")
    scala.tools.nsc.Main.main(args);
  }
}
