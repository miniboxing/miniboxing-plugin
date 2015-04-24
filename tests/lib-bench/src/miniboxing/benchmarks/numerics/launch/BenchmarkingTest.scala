package miniboxing.benchmarks.numerics.launch

import scalameter._
import tests._
import miniboxing.runtime.MiniboxedTuple
import miniboxing.runtime.MiniboxConstants
import miniboxing.runtime.MiniboxConversions
import miniboxing.runtime.MiniboxConversionsDouble

object BenchmarkingTest extends ScalameterBenchTest
                           with GenericBenchTest
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
    List(500)
  }

  withTestSize(500){
    val random = new util.Random(0)

    val (h, w) = (testSize, testSize)
    val matrix: Array[Array[Double]] = new Array(w)
    for( i <- 0 to (testSize - 1)){
      val mrow: Array[Double] = new Array(h)
      for( j <- 0 to (testSize - 1)){
        mrow(j) = random.nextDouble()
      }
      matrix(i) = mrow
    }
    
    // run the tests:
    testHardcodedMiniboxed(matrix, h, w)
    testIdeal(matrix, h, w)
    testGeneric(matrix, h, w)
  }
}
