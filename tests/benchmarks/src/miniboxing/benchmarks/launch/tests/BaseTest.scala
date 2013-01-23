package miniboxing.benchmarks.launch.tests

trait BaseTest {

  def testSize: Int
  def withTestSize(size: Int)(f : => Unit) // temporarely override test size
  def test[T](transformation: String, tag: String, setup: Int => Unit, benchmark: => Unit, teardown: => Unit)
}
