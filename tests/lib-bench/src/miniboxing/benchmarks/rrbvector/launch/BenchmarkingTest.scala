package miniboxing.benchmarks.rrbvector.launch

import scalameter._
import tests._

object BenchmarkingTest extends ScalameterBenchTest
                           with GenericBenchTest
                           with MiniboxedBenchTest
                           with Serializable {

  // the number of independent samples to use
  lazy val sampleCount = 2

  // the command used to start the JVM
  // HotSpot:
  lazy val javaCommand = "java -server -Xmx4g -Xms4g -Xss4m"
  lazy val javaPreJDK7 = false

  // the test size
  lazy val testSizes = {
    List(5000000)
  }

  withTestSize(5000000){

    // run the tests:
    testMiniboxed()
    testGeneric()
  }
}
