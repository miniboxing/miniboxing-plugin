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
  lazy val sampleCount = 20

  // the command used to start the JVM
  // HotSpot:
   lazy val javaCommand = "java -server"
   lazy val javaPreJDK7 = false
  // JRockit:
  // lazy val javaCommand = "/home/sun/workspace/dev/jrockit/bin/java -jrockit"
  // lazy val javaPreJDK7 = true
  // Graal:
//  lazy val javaCommand = "/home/sun/workspace/dev/graal/bin/java -graal -dsa"
//  lazy val javaPreJDK7 = true
  // NOTE: Seems Graal can't handle it:
  //  Bootstrapping Graal............................ in 75405 ms
  //  Bootstrapping Graal............................ in 72693 ms
  //                       ideal.array.insert :  Parameters(size -> 1000):    5.85742
  //  Bootstrapping Graal#
  //  # A fatal error has been detected by the Java Runtime Environment:
  //  #
  //  #  SIGSEGV (0xb) at pc=0x00007fb8b0e2feac, pid=24440, tid=140431047948032
  //  #
  //  # JRE version: Java(TM) SE Runtime Environment (7.0_25-b15) (build 1.7.0_25-b15)
  //  # Java VM: OpenJDK 64-Bit Graal VM (25.0-b37-internal mixed mode linux-amd64 compressed oops)
  //  # Problematic frame:
  //  # V  [libjvm.so+0x3faeac]  oopDesc* PSPromotionManager::copy_to_survivor_space<false>(oopDesc*)+0x4c
  //  #
  //  # Failed to write core dump. Core dumps have been disabled. To enable core dumping, try "ulimit -c unlimited" before starting Java again
  //  #
  //  # An error report file with more information is saved as:
  //  # /mnt/data-local/Work/Workspace/dev/graal/hs_err_pid24440.log
  //  #
  //  # If you would like to submit a bug report, please visit:
  //  #   http://bugreport.sun.com/bugreport/crash.jsp
  //  #
  //  Error running separate JVM: java.lang.ClassCastException: org.scalameter.execution.SeparateJvmsExecutor$$anonfun$sample$1$1 cannot be cast to scala.collection.Seq
  //  Classpath: /home/sun/.sbt/.lib/0.12.2/sbt-launch.jar

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


