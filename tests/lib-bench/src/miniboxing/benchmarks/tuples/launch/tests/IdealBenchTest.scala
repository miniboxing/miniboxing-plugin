package miniboxing.benchmarks.tuples.launch.tests

import miniboxing.benchmarks.tuples.ideal.TuplesQuickSort
import scala.util.Random

trait IdealBenchTest extends BaseTest {
  
  def testIdeal() = {
    var arr: Array[(Int, Double)] = new Array[(Int, Double)](testSize)
    val random = new Random(0)
    for( ind <- 0 to (testSize - 1)){
       arr(ind) = (random.nextInt(testSize), 0.0)
    }

    test(
        "ideal", 
        "quicksort_array_of_tuples", 
        _ => (),  
        TuplesQuickSort.quicksortByKey(arr)(new Ordering[Int]{
          override def compare(a: Int, b: Int) = a - b
        }), 
        () => {}
     )
  }
}
