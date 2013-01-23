package miniboxing.launch

object Main {
  def main(args: Array[String]) {
    var args = Array("-Xplugin:lib/minibox.jar",
      "-cp",
      "bin/:test-bin/",
      "-d",
      "test-bin/",
      "test/benchmarks/Benchmark.scala",
      "test/benchmarks/MBList.scala",
      "test/benchmarks/MBResizableArray.scala",
      "-optimize",
      "-Ylog:inline",
      //"-Xprint:all",
      //          "-Ybrowse:refchecks",
      "-Xprint:minibox,cleanup",
      //"-Ystop-after:genicode",
      "-no-specialization",
      "-nowarn")
    scala.tools.nsc.Main.main(args);
  }
}

