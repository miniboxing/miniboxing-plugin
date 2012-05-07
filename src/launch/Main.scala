package launch

object Main {
  def main(args: Array[String]) {
    val args = Array("-Xplugin:lib/minibox.jar", 
          "-cp", 
          "bin/", 
//          "test/simple/Cell.scala", 
          "test/simple/Members.scala", 
          "-Xprint:minibox,cleanup",
          "-Ystop-after:cleanup",
          "-no-specialization",
          "-nowarn"
          )
    println("running")
    scala.tools.nsc.Main.main(args);
  }
}

