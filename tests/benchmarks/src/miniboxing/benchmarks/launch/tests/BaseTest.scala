package miniboxing.benchmarks.launch.tests

trait BaseTest {

  def testSize: Int
  def megamorphicTestSize: Int // may be less than the testSize, but should be above 10000
  def withTestSize(size: Int)(f : => Unit) // temporarely override test size
  def test[T](transformation: String, tag: String, setup: Int => Unit, benchmark: => Unit, teardown: => Unit)
}
