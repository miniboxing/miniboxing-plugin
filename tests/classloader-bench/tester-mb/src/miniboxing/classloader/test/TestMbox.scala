package miniboxing.classloader.test

object TestMboxing {
  def main(args: Array[String]): Unit = {
    println("Test Miniboxing")
    val classloader = miniboxing.classloader.MiniboxingClassLoader.classloader(TestMboxing.this, verbose = sys.props.get("classloader.debug").isDefined)
    var time = System.currentTimeMillis()
    var i = 0
    while (i < 9) {
      classloader.findClass("miniboxing.benchmarks.collection.immutable.Vector_class_" + i)
      // if classes are dumped:
      //Class.forName("miniboxing.benchmarks.collection.immutable.Vector_class_" + i)
      i += 1
    }
    time -= System.currentTimeMillis()
    time = -time
    println("Time: " + time + "ms")
  }
}
