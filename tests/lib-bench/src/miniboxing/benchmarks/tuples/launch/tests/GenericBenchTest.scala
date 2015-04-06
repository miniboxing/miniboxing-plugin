package miniboxing.benchmarks.tuples.launch.tests

import miniboxing.benchmarks.tuples.generic.TuplesQuickSort
import scala.util.Random

trait GenericBenchTest extends BaseTest {

  def testGeneric(arr: Array[(Int, Double)]) = {
    test("generic", "quicksort_array_of_tuples", _ => (), TuplesQuickSort.quicksortByKey[Int](arr.clone()), () => {})
  }
}
