package miniboxing.classloader.test

import miniboxing.benchmarks.collection.immutable._
import miniboxing.runtime.MiniboxConstants._

object TestNoSp {
  def main(args: Array[String]): Unit = {
    println("Test Generic")
    var time = System.currentTimeMillis()
    Class.forName("miniboxing.benchmarks.collection.immutable.Vector")
    time -= System.currentTimeMillis()
    time = -time
    println("Time: " + time + "ms")
  }
}

