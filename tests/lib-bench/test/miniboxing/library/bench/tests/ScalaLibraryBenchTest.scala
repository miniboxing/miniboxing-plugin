package miniboxing.library.bench.tests

import org.scalameter.api._

trait ScalaLibraryBenchTest extends BaseTest {

  private[this] object TestList {
    def list_foldLeft(l: List[Float]): Double =
      l.foldLeft(0.0: Double)((x1: Double, x2: Float) => x1 + x2)
  }

  def testScalaLibrary() = {
    import TestList._

    val transformation = "generic"

    var l: List[Float] = null
    var r: Double = 0
    test(transformation, "list.fold_left",
        setup =      testSize => { l = (1 to testSize).map(_.toFloat).toList },  // ran once, before starting the test
        benchmark =  r = list_foldLeft(l),                                       // what is benchmarked
        teardown =   () => { assert(r - l.sum <= 0.01); l = null })              // ran once, after benchmarking is done
  }
}
