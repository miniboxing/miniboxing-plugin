package miniboxing.benchmarks.tuples.launch.tests

import miniboxing.benchmarks.tuples.generic.TuplesQuickSort
import scala.util.Random

trait GenericBenchTest extends BaseTest {
  
  def testGeneric() = {
    var arr: Array[(Int, Double)] = new Array[(Int, Double)](testSize)
    val random = new Random(0)
    for( ind <- 0 to (testSize - 1)){
       arr(ind) = (random.nextInt(testSize), 0.0)
    }

    test("generic", "quicksort_array_of_tuples", _ => (), TuplesQuickSort.quicksortByKey[Int](arr), () => {})
  }
}
