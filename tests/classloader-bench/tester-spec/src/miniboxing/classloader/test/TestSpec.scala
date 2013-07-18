package miniboxing.classloader.test

import miniboxing.benchmarks.collection.immutable._

object TestSpec {
  def main(args: Array[String]): Unit = {
    println("Test Specialization")
    var specs = Array[Char]('Z', 'B', 'C', 'D', 'F', 'I', 'J', 'S', 'V')
    var time = System.currentTimeMillis()
    var i = 0
    while (i < 9) {
      Class.forName("miniboxing.benchmarks.collection.immutable.Vector$mc" + specs(i).toString + "$sp")
      i += 1
    }
    time -= System.currentTimeMillis()
    time = -time
    println("Time: " + time + "ms")
  }
}
