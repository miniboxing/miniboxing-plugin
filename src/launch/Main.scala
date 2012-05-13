package launch

object Main {
  def main(args: Array[String]) {
    val args = Array("-Xplugin:lib/minibox.jar", 
          "-cp", 
          "bin/", 
          "test/simple/Pair.scala", 
//          "test/simple/ArrayWrap.scala", 
//          "test/simple/Members.scala", 
          "-Xprint:minibox,cleanup",
          "-Ystop-after:genicode",
          "-no-specialization",
          "-nowarn"
          )
    println("running")
    scala.tools.nsc.Main.main(args);
  }
}

