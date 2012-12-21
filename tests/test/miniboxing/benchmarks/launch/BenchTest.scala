package miniboxing.benchmarks.launch

import org.scalameter.{log, CurveData}
import org.scalameter.api._
import miniboxing.benchmarks.generic._

trait BenchTest extends PerformanceTest with Serializable {

  // PerformanceTest defs
  // TODO: Not sure this is supposed to be transient, but it complains class is not serializable due to it
  @transient lazy val executor = SeparateJvmsExecutor(
    Executor.Warmer.Default(),
    Aggregator.average,
    new Executor.Measurer.Default
  )
  // Finally a less verbose reporter!
  def reporter = new LoggingReporter {
    override def report(result: CurveData, persistor: Persistor) {
      var output = f"${result.context.scope}%40s:  "
      for (measurement <- result.measurements) {
        output += f"${measurement.params}: ${measurement.time}% 10.5f  "
      }
      println(output)
    }
  }
  def persistor = Persistor.None

  // And the common benchmarking stuff:
  var N = 0
  val sizes = Gen.single("size")(1000000)

  def test[T](clazz: String, method: String, setup: Int => Unit, benchmark: => Unit, teardown: => Unit) =
    performance of clazz in {
      measure method method in {
        using(sizes) config (
          exec.independentSamples -> 5
        ) setUp {
          size => N = size; setup(size)
        } tearDown {
          size => N = 0; teardown
        } in {
          size => benchmark
        }
      }
    }
}
