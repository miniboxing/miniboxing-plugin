package launch

object Main {
  def main(args: Array[String]) {
    
    val args = new Array[String](1)
    args(0) = "test/Test.scala"
      
    println("running")
    
    scala.tools.nsc.Main.main(args);
  }
}