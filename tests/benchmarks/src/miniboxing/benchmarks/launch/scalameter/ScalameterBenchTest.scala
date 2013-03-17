package miniboxing.benchmarks.launch.scalameter
import org.scalameter.CurveData
import org.scalameter.api._
import miniboxing.benchmarks.launch.tests.BaseTest
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

trait ScalameterBenchTest extends PerformanceTest
                             with BaseTest {

  import collection.mutable.{HashSet, HashMap}

  var testSize: Int = _
  def withTestSize(size: Int)(f : => Unit) = {
    val _testSize = testSize
    testSize = size
    f
    testSize = _testSize
  }
  def testSizes: List[Int]
  val sizes = Gen.enumeration("size")(testSizes: _*)
  def lastTag: String
  def lastTraf: String

  // Store the test values
  var testTrafs = List[String]()
  var testTags  = List[String]()

  // testTag -> testTraf -> size -> (time, error)
  val testValues = HashMap[String, HashMap[String, HashMap[Int, (Double, Double)]]]()

  // PerformanceTest defs
  // TODO: Not sure this is supposed to be transient, but it complains class is not serializable due to it
  @transient lazy val executor = SeparateJvmsExecutor(
    Executor.Warmer.Default(),
    Aggregator.complete(Aggregator.average),
    new Executor.Measurer.Default
  )

  // Finally a less verbose reporter!
  def reporter = new LoggingReporter {
    override def report(result: CurveData, persistor: Persistor) {
      var output = f"${result.context.scope}%40s:  "

      val testTitle = result.context.scope.toString
      val testTag   = testTitle.substring(testTitle.indexOf('.') + 1).trim          // the transformation being applied
      val testTraf  = testTitle.substring(0, testTitle.indexOf('.')).trim     // the tag of the test (what it tests)

      for (measurement <- result.measurements) {
        output += f"${measurement.params}: ${measurement.time}% 10.5f  "

        // fill in the table for getting final results
        val testSize  = measurement.params.axisData("size").toString.toInt     // the size of the test
        if (!testTrafs.contains(testTraf)) testTrafs ::= testTraf
        if (!testTags.contains(testTag))   testTags  ::= testTag
        val testEntry = testValues.getOrElseUpdate(testTag, HashMap()).getOrElseUpdate(testTraf, HashMap())
        testEntry += testSize -> (measurement.time, measurement.errors.sdeviation)

        // println(s""""${testTag}" "${testTraf}", "${testSize}": ${measurement.time} +/- ${measurement.errors.sdeviation}""")
      }

      println(output)

      // printout:
      if ((testTag == lastTag) && (testTraf == lastTraf))
        printResults()
    }
  }

  def persistor = Persistor.None

  def test[T](transformation: String, tag: String, setup: Int => Unit, benchmark: => Unit, teardown: => Unit) = {
    performance of transformation in {
      measure method tag in {
        using(sizes) config (exec.independentSamples -> 5/*, exec.jvmflags -> "-Xint"*/) setUp {
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
        println(f"${testTraf}%30s : " + entries.map({case (value, error) => f"${value}%11.5f +/- ${error}%9.5f "}).mkString("  "))
      }
    }
    println("\n\n\n")
  }
}
