package miniboxing.classloader.test

import miniboxing.benchmarks.collection.immutable._

object TestSpec {
  def main(args: Array[String]): Unit = {
    println("Test Specialization")
    new Vector[Int](0, 1, 0)
  }
}
