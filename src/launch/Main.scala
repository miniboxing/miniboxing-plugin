package launch

object Main {
  def main(args: Array[String]) {
    var args = Array("-Xplugin:lib/minibox.jar", 
          "-cp", 
          "bin/:test-bin/", 
          "-d",
          "test-bin/",
//          "test/simple/MBList.scala", 
          "test/simple/ArrayWrap.scala", 
//          "-optimize",
          //"-Xprint:all",
//          "-Ybrowse:refchecks",
          "-Xprint:minibox,cleanup",
          //"-Ystop-after:genicode",
          "-no-specialization",
          "-nowarn"
          )
    scala.tools.nsc.Main.main(args);
  }
}

