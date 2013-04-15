package miniboxing.library.bench.scalameter
import org.scalameter.CurveData
import org.scalameter.api._

import miniboxing.library.bench.tests.BaseTest

trait ScalameterBenchTest extends PerformanceTest with BaseTest {

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

  // PerformanceTest defs
  def executor = SeparateJvmsExecutor(
    Executor.Warmer.Default(),
    Aggregator.complete(Aggregator.average),
    new Executor.Measurer.Default
  )

  // Finally a less verbose reporter!
  def reporter = new LoggingReporter {
    override def report(result: CurveData, persistor: Persistor) {
      // add test title:
      var output = f"${result.context.scope}%40s:  "

      for (measurement <- result.measurements)
        // add, for each size, the results:
        output += f"${measurement.params}: ${measurement.time}% 10.5f +/- ${measurement.errors.sdeviation}% 10.5f"

      // print the result:
      println(output)
    }
  }

  def persistor = Persistor.None

  def test[T](transformation: String, tag: String, setup: Int => Unit, benchmark: => Unit, teardown: => Unit) = {
    performance of transformation in {
      measure method tag in {
        using(sizes) config (exec.independentSamples -> 5) setUp {
          size => testSize = size; System.gc(); setup(size); System.gc();
        } tearDown {
          teardown; size => testSize = 0
        } in {
          size => benchmark
        }
      }
    }
  }
}
