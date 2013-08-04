package miniboxing.benchmarks.launch

import scalameter.ScalameterBenchTest
import tests._

object BenchmarkingTest extends ScalameterBenchTest
                           with GenericBenchTest
                           with SpecializedBenchTest
                           with HardcodedMiniboxingSimpleFS // fullswitch
                           with HardcodedMiniboxingSimpleSS // semiswitch
                           with HardcodedMiniboxingSimpleDT // decision trees
                           with HardcodedMiniboxingSimpleLI // linear
                           with HardcodedMiniboxingSimpleNI // no inline
                           with HardcodedMiniboxingSimpleCL
                           with HardcodedMiniboxingDispatcherBenchTest
                           with HardcodedMiniboxingDispatcherBenchTestCL
                           with IdealBenchTest
                           with Serializable{

  // the number of independent samples to use
  lazy val sampleCount = 2

  // the command used to start the JVM
  // HotSpot:
  lazy val javaCommand = "java -server"
  lazy val javaPreJDK7 = false
  // JRockit:
  //  lazy val javaCommand = "/home/sun/workspace/dev/jrockit/bin/java -jrockit -d64"
  //  lazy val javaPreJDK7 = true
  // Graal:
  //  lazy val javaCommand = "/home/sun/workspace/dev/graal/bin/java -graal -d64 -XX:+BootstrapGraal"
  //  lazy val javaPreJDK7 = true

  // the test size
  lazy val testSizes = {
    //List(1000, 2000, 3000)
    List(1000000, 2000000, 3000000)
    //List(1000)
  }
  def megamorphicTestSize = 200000
  def lastTag = "list.find"
  def lastTraf = "generic mega"

  // run the tests:
  testIdeal()
  testHardcodedMiniboxingDispatch(false)
  testHardcodedMiniboxingDispatch(true)
  testHardcodedMiniboxingDispatchClassLoader(false)
  testHardcodedMiniboxingDispatchClassLoader(true)
  testHardcodedMiniboxingSimpleFS(false)
  testHardcodedMiniboxingSimpleFS(true)
  testHardcodedMiniboxingSimpleSS(false)
  testHardcodedMiniboxingSimpleSS(true)
  testHardcodedMiniboxingSimpleDT(false)
  testHardcodedMiniboxingSimpleDT(true)
  testHardcodedMiniboxingSimpleLI(false)
  testHardcodedMiniboxingSimpleLI(true)
  testHardcodedMiniboxingSimpleNI(false)
  testHardcodedMiniboxingSimpleNI(true)
  testHardcodedMiniboxingSimpleClassLoader(false)
  testHardcodedMiniboxingSimpleClassLoader(true)
  testSpecialized(false)
  testSpecialized(true)
  testGeneric(false)
  testGeneric(true)
}


