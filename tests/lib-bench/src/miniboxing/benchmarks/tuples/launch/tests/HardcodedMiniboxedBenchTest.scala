package miniboxing.benchmarks.tuples.launch.tests

import scala.util.Random

import miniboxing.benchmarks.tuples.miniboxed.hardcoded.TuplesQuickSort
import miniboxing.runtime.MiniboxConstants
import miniboxing.runtime.MiniboxConversions
import miniboxing.runtime.MiniboxConversionsDouble
import miniboxing.runtime.MiniboxedTuple

trait HardcodedMiniboxedBenchTest extends BaseTest {

  def testHardcodedMiniboxed(arr: Array[(Int, Double)]) = {
    test(
      "miniboxed_hardcoded-" + arr.length,
      "quicksort_array_of_tuples",
      _ => (),
      TuplesQuickSort.quicksortByKeyLong[Int](arr.clone())(TuplesQuickSort.IntMiniboxedOrdering),
      () => {}
    )
  }
}
