package miniboxing.benchmarks.streams.launch

import scalameter._
import tests._

object BenchmarkingTest extends ScalameterBenchTest
    with GenericBenchTest
    with MiniboxedBenchTest
    with Serializable {

  // the number of independent samples to use
  lazy val sampleCount = 10

  // the command used to start the JVM
  // HotSpot:
  lazy val javaCommand = "java -server -Xmx2g -Xms2g -Xss4m"
  lazy val javaPreJDK7 = false

  // the test size
  lazy val testSizes = {
    List(10000000)
  }

  withTestSize(10000000) {
    // run the tests:
    testGeneric()
    testMiniboxed()
    //testMiniboxedWithOutFunctions() ??
  }
}
