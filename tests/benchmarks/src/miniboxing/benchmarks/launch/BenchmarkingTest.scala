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

  lazy val baseDir = 
    java.net.InetAddress.getLocalHost().getHostName() match {
      case "sun-laptop" => "/home/sun/workspace/dev/"
      case "lamppc47"   => "/localhome/ureche/software/"
      case _            => "./" // uh-oh, I don't know
    }

  // the command used to start the JVM
  // HotSpot:
  lazy val javaCommand = "java -server"
  lazy val javaPreJDK7 = false
  // JRockit:
  //  lazy val javaCommand = baseDir + "jrockit/bin/java -jrockit -d64 -Xms4g -Xmx4g -Xss4m"
  //  lazy val javaPreJDK7 = true
  // Graal:
  //  lazy val javaCommand = baseDir + "graal/bin/java -graal -dsa -Xmx8g -Xms8g -Xss10m -d64 -XX:+BootstrapGraal"
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
//  testHardcodedMiniboxingSimpleSS(false)
//  testHardcodedMiniboxingSimpleSS(true)
//  testHardcodedMiniboxingSimpleDT(false)
//  testHardcodedMiniboxingSimpleDT(true)
//  testHardcodedMiniboxingSimpleLI(false)
//  testHardcodedMiniboxingSimpleLI(true)
//  testHardcodedMiniboxingSimpleNI(false)
//  testHardcodedMiniboxingSimpleNI(true)
  testHardcodedMiniboxingSimpleClassLoader(false)
  testHardcodedMiniboxingSimpleClassLoader(true)
  testSpecialized(false)
  testSpecialized(true)
  testGeneric(false)
  testGeneric(true)
}


