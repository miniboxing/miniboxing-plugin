package miniboxing.benchmarks.tuples.launch.tests

import miniboxing.benchmarks.tuples.specialized.TuplesQuickSort
import scala.util.Random

trait SpecializedBenchTest extends BaseTest {

  def testSpecialized(arr: Array[(Int, Double)]) = {
    test(
      "specialized",
      "quicksort_array_of_tuples",
      _ => (),
      TuplesQuickSort.quicksortByKey(arr.clone())(TuplesQuickSort.IntSpecializedOrdering),
      () => {}
     )
  }
}
