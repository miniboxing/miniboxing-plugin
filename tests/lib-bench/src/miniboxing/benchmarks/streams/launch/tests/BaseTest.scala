package miniboxing.benchmarks.streams.launch.tests

trait BaseTest {
  def testSize: Int
  def withTestSize(size: Int)(f: => Unit)
  def test[T](transformation: String, tag: String, setup: Int => Unit, benchmark: => Unit, teardown: => Unit, extraJVMFlags: List[String] = Nil)
}
