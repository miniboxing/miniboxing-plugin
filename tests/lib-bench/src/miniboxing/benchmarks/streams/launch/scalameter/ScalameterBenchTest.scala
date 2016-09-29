package miniboxing.benchmarks.streams.launch.scalameter

import scala.collection.mutable.HashMap

import org.scalameter.CurveData
import org.scalameter.Key
import org.scalameter.api.Aggregator
import org.scalameter.api.Executor
import org.scalameter.api.Gen
import org.scalameter.api.LoggingReporter
import org.scalameter.api.PerformanceTest
import org.scalameter.api.Persistor
import org.scalameter.api.SeparateJvmsExecutor
import org.scalameter.api.exec
import org.scalameter.utils.Tree

import miniboxing.benchmarks.tuples.launch.tests.BaseTest

trait ScalameterBenchTest extends PerformanceTest
    with BaseTest
    with java.io.Serializable {

  import collection.mutable.{ HashSet, HashMap }

  var testSize: Int = _
  def withTestSize(size: Int)(f: => Unit) = {
    val _testSize = testSize
    testSize = size
    f
    testSize = _testSize
  }

  def testSizes: List[Int]
  val sizes = Gen.enumeration("size")(testSizes: _*)

  def sampleCount: Int
  def javaCommand: String
  def javaPreJDK7: Boolean

  // Store the test values
  var testTrafs = List[String]()
  var testTags = List[String]()

  // testTag -> testTraf -> size -> (time, error)
  val testValues = HashMap[String, HashMap[String, HashMap[Int, (Double, Double)]]]()

  // PerformanceTest defs
  // TODO: Not sure this is supposed to be transient, but it complains class is not serializable due to it
  def executor = SeparateJvmsExecutor(
    new Executor.Warmer.Default(),
    Aggregator.average,
    new Executor.Measurer.Default)

  // Finally a less verbose reporter!
  def reporter = new LoggingReporter {
    override def report(result: CurveData, persistor: Persistor) {
      var output = f"${result.context.scope}%40s:  "

      val testTitle = result.context.scope.toString
      val testTag = testTitle.substring(testTitle.indexOf('.') + 1).trim // the transformation being applied
      val testTraf = testTitle.substring(0, testTitle.indexOf('.')).trim // the tag of the test (what it tests)

      for (measurement <- result.measurements) {
        output += f"${measurement.params}: ${measurement.value}% 10.5f  "

        // fill in the table for getting final results
        val testSize = measurement.params.axisData("size").toString.toInt // the size of the test
        if (!testTrafs.contains(testTraf)) testTrafs ::= testTraf
        if (!testTags.contains(testTag)) testTags ::= testTag
        val testEntry = testValues.getOrElseUpdate(testTag, HashMap()).getOrElseUpdate(testTraf, HashMap())
        testEntry += testSize -> ((measurement.value, measurement.errors.sdeviation))

        // println(s""""${testTag}" "${testTraf}", "${testSize}": ${measurement.time} +/- ${measurement.errors.sdeviation}""")
      }

      println(output)
    }

    override def report(result: Tree[CurveData], persistor: Persistor) = {
      printResults()
      true
    }
  }

  def persistor = Persistor.None

  def test[T](transformation: String, tag: String, setup: Int => Unit, benchmark: => Unit, teardown: => Unit, extraJVMFlags: List[String] = Nil) = {
    performance of transformation in {
      measure method tag in {
        using(sizes) config (exec.independentSamples -> sampleCount,
          exec.jvmcmd -> javaCommand,
          Key.preJDK7 -> javaPreJDK7,
          exec.jvmflags -> extraJVMFlags.mkString(" ")) setUp {
            size => testSize = size; System.gc(); setup(size); System.gc();
          } tearDown {
            teardown; size => testSize = 0
          } in {
            size => benchmark
          }
      }
    }
  }

  def printResults() = {
    println("\n\n\n")
    for (testTag <- testTags.reverse) {
      println(testTag + ":")
      for (testTraf <- testTrafs.reverse) {
        val tagEntry = testValues.get(testTag).flatMap(_.get(testTraf))
        val entries = testSizes.map(size => tagEntry.flatMap(_.get(size)).getOrElse((0.0, 0.0)))
        println(f"${testTraf}%30s : " + entries.map({ case (value, error) => f"${value}%11.5f +/- ${error}%9.5f " }).mkString("  "))
      }
    }
    println("\n\n\n")
  }
}
