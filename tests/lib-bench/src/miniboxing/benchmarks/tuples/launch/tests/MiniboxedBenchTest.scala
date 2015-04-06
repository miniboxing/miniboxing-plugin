package miniboxing.benchmarks.tuples.launch.tests

import scala.util.Random
import miniboxing.benchmarks.tuples.miniboxed.TuplesQuickSort

trait MiniboxedBenchTest extends BaseTest {

  def testMiniboxed(arr: Array[(Int, Double)]) = {
    test(
      "miniboxed",
      "quicksort_array_of_tuples",
      _ => (),
      TuplesQuickSort.quicksortByKey[Int](arr.clone())(TuplesQuickSort.IntMiniboxedOrdering),
      () => {}
    )
  }
}
