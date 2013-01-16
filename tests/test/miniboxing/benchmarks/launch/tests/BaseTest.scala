package miniboxing.benchmarks.launch.tests

trait BaseTest {

  def testSize: Int
  def test[T](transformation: String, tag: String, setup: Int => Unit, benchmark: => Unit, teardown: => Unit)
}
