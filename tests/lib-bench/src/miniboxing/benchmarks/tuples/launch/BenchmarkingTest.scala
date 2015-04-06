package miniboxing.benchmarks.tuples.launch

import scalameter._
import tests._
import miniboxing.runtime.MiniboxedTuple
import miniboxing.runtime.MiniboxConstants
import miniboxing.runtime.MiniboxConversions
import miniboxing.runtime.MiniboxConversionsDouble

object BenchmarkingTest extends ScalameterBenchTest
                           with GenericBenchTest
                           with SpecializedBenchTest
                           with IdealBenchTest
                           with HardcodedMiniboxedBenchTest
                           with MiniboxedBenchTest
                           with Serializable {

  // the number of independent samples to use
  lazy val sampleCount = 2

  // the command used to start the JVM
  // HotSpot:
  lazy val javaCommand = "java -server"
  lazy val javaPreJDK7 = false

  // the test size
  lazy val testSizes = {
    List(1000000)
  }

  withTestSize(1000000){
    var arr: Array[(Int, Double)] = new Array[(Int, Double)](testSize)
    val random = new util.Random(0)
    for( ind <- 0 to (testSize - 1)){
      arr(ind) = (random.nextInt(testSize), 0.0)
//      // alternative approach, should produce the same result:
//      arr(ind) = MiniboxedTuple.newTuple2_long_double[Int, Double](
//        MiniboxConstants.INT,
//        MiniboxConstants.DOUBLE,
//        MiniboxConversions.int2minibox(random.nextInt(testSize)),
//        MiniboxConversionsDouble.double2minibox(0.0)
//      )
    }

    // run the tests:
    testHardcodedMiniboxed(arr)
    testMiniboxed(arr)
    testIdeal(arr)
    testGeneric(arr)
    testSpecialized(arr)
  }
}
