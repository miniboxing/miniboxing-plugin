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
                           with SpecializedBenchTest
                           with Serializable {

  // the number of independent samples to use
  lazy val sampleCount = 2

  // the command used to start the JVM
  // HotSpot:
  lazy val javaCommand = "java -server"
  lazy val javaPreJDK7 = false

  // the test size
  lazy val testSizes = {
    List(1000)
  }

  withTestSize(1000){
    val random = new util.Random(0)

    val (h, w) = (testSize, testSize)
    val matrixD: Array[Array[Double]] = new Array(w)
    val matrixI: Array[Array[Int]] = new Array(w)
    val matrixL: Array[Array[Long]] = new Array(w)
    val matrixS: Array[Array[Short]] = new Array(w)
    for( i <- 0 to (testSize - 1)){
      val mrowD: Array[Double] = new Array(h)
      val mrowI: Array[Int] = new Array(h)
      val mrowL: Array[Long] = new Array(h)
      val mrowS: Array[Short] = new Array(h)
      for( j <- 0 to (testSize - 1)){
        mrowD(j) = random.nextDouble()
        mrowI(j) = random.nextInt()
        mrowL(j) = random.nextLong()
        mrowS(j) = random.nextInt().toShort
      }
      matrixD(i) = mrowD
      matrixI(i) = mrowI
      matrixL(i) = mrowL
      matrixS(i) = mrowS
    }
    
//    // run the tests:
//    testHardcodedMiniboxed(matrixD, matrixI, matrixL, matrixS, h, w, h/5, w/5)
//    testIdeal(matrixD, h, w, h/5, w/5)
//    testGeneric(matrixD, matrixI, matrixL, matrixS, h, w, h/5, w/5)
    testSpecialized(matrixD, matrixI, matrixL, matrixS, h, w, h/5, w/5)
  }
}
