package miniboxing.benchmarks.tuples.launch.tests

import scala.util.Random
import miniboxing.benchmarks.tuples.miniboxed.TuplesQuickSort

trait MiniboxedBenchTest extends BaseTest {
  
  def testMiniboxed() = {
    var arr: Array[(Int, Double)] = new Array[(Int, Double)](testSize)
    val random = new Random(0)
    for( ind <- 0 to (testSize - 1)){
       arr(ind) = (random.nextInt(testSize), 0.0)
    }

    test(
      "miniboxed", 
      "quicksort_array_of_tuples", 
      _ => (),  
      TuplesQuickSort.quicksortByKey[Int](arr)(TuplesQuickSort.IntMiniboxedOrdering), 
      () => {}
    )
  }
}
