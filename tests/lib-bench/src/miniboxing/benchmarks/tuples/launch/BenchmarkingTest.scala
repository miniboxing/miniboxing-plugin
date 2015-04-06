package miniboxing.benchmarks.tuples.launch

import scalameter._
import tests._

object BenchmarkingTest extends ScalameterBenchTest
                           with GenericBenchTest
                           with SpecializedBenchTest
                           with IdealBenchTest
                           with HardcodedMiniboxedBenchTest
                           with Serializable {

  // the number of independent samples to use
  lazy val sampleCount = 2

  // the command used to start the JVM
  // HotSpot:
  lazy val javaCommand = "java -server"
  lazy val javaPreJDK7 = false

  // the test size
  lazy val testSizes = {
    List(2000000)
  }
  
  withTestSize(2000000){
    // run the tests:
    testIdeal()
    testGeneric()
    testSpecialized()
    testHardcodedMiniboxed()
  }
}
