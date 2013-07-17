package miniboxing.classloader.test

import miniboxing.benchmarks.collection.immutable._
import miniboxing.runtime.MiniboxConstants._

object TestMboxing {
  def main(args: Array[String]): Unit = {
    println("Test Miniboxing")
    new Vector_class_J[Int](INT, 0, 1, 0)
  }
}
