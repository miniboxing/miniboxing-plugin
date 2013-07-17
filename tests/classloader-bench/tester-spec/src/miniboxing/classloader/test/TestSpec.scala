package miniboxing.classloader.test

import miniboxing.benchmarks.collection.immutable._

object TestSpec {
  def main(args: Array[String]): Unit = {
    println("Test Specialization")
    var time = System.currentTimeMillis()
    new Vector[Unit](0, 1, 0)
    new Vector[Boolean](0, 1, 0)
    new Vector[Byte](0, 1, 0)
    new Vector[Char](0, 1, 0)
    new Vector[Short](0, 1, 0)
    new Vector[Int](0, 1, 0)
    new Vector[Long](0, 1, 0)
    new Vector[Float](0, 1, 0)
    new Vector[Double](0, 1, 0)
    time -= System.currentTimeMillis()
    time = -time
    println("Time in miliseconds: " + time)
  }
}
