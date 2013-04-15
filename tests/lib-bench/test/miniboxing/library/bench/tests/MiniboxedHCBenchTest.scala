package miniboxing.library.bench.tests

import org.scalameter.api._

trait MiniboxedHCBenchTest extends BaseTest {

  import miniboxing.library.bench.MiniboxedHC._

  private[this] object TestList {
    def list_foldLeft(l: List[Float]): Double =
      l.foldLeft(0.0: Double)((x1: Double, x2: Float) => x1 + x2)
  }

  def testMiniboxedHCLibrary() = {
    import TestList._

    val transformation = "generic"

    var l: List[Float] = null
    var r: Double = 0
    test(transformation, "list.fold_left",
        setup =      testSize => { l = getList(testSize) },
        benchmark =  r = list_foldLeft(l),
        teardown =   () => { assert(r - l.sum <= 0.01); l = null })
  }
}
