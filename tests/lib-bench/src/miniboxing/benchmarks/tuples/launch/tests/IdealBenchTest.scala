package miniboxing.benchmarks.tuples.launch.tests

import miniboxing.benchmarks.tuples.ideal.TuplesQuickSort
import scala.util.Random

trait IdealBenchTest extends BaseTest {

  def testIdeal(arr: Array[(Int, Double)]) = {
    test(
      "ideal",
      "quicksort_array_of_tuples",
      _ => (),
      TuplesQuickSort.quicksortByKey(arr.clone())(TuplesQuickSort.IntIdealOrdering),
      () => {}
     )
  }
}
