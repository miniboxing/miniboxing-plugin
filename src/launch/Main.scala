package launch

object Main {
  def main(args: Array[String]) {

    val args = Array("test/Test.scala")

    println("running")

    scala.tools.nsc.Main.main(args);
  }
}