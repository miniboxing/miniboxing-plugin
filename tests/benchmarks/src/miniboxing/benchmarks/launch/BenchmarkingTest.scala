package miniboxing.benchmarks.launch

import scalameter.ScalameterBenchTest
import tests._

object BenchmarkingTest extends ScalameterBenchTest
                           with GenericBenchTest
                           with SpecializedBenchTest
                           with HardcodedMiniboxingSimple
                           with HardcodedMiniboxingSimpleCL
                           with HardcodedMiniboxingDispatcherBenchTest
                           with HardcodedMiniboxingDispatcherBenchTestCL
                           with IdealBenchTest
                           with Serializable{

  lazy val testSizes = {
    //List(1000, 2000, 3000)
    List(1000000, 2000000, 3000000)
//    List(100000)
  }
  def lastTag = "list.find"
  def lastTraf = "generic"

  // run the tests:
  testIdeal()
  testHardcodedMiniboxingDispatch(false)
  testHardcodedMiniboxingDispatch(true)
  testHardcodedMiniboxingDispatchClassLoader(false)
  testHardcodedMiniboxingDispatchClassLoader(true)
  testHardcodedMiniboxingSimple(false)
  testHardcodedMiniboxingSimple(true)
  testHardcodedMiniboxingSimpleClassLoader(false)
  testHardcodedMiniboxingSimpleClassLoader(true)
  testSpecialized(false)
  testSpecialized(true)
  testGeneric(false)
  testGeneric(true)
}


