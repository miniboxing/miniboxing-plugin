package miniboxing.tests.compile.bug162c

abstract class PerformanceTest
trait HTMLReport extends PerformanceTest
trait OfflineRegressionReport extends HTMLReport
trait BaseVectorGenerator[A]
trait BaseVectorBenchmark[A] extends OfflineRegressionReport with BaseVectorGenerator[A]
abstract class AppendBenchmarks[A] extends BaseVectorBenchmark[A] {
  self: PerformanceTest =>
}
