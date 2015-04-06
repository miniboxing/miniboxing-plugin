package miniboxing.benchmarks.tuples.launch.tests

import miniboxing.benchmarks.tuples.specialized.TuplesQuickSort
import scala.util.Random

trait SpecializedBenchTest extends BaseTest {
  
  def testSpecialized() = {
    var arr: Array[(Int, Double)] = new Array[(Int, Double)](testSize)
    val random = new Random(0)
    for( ind <- 0 to (testSize - 1)){
       arr(ind) = (random.nextInt(testSize), 0.0)
    }

    test(
        "specialized", 
        "quicksort_array_of_tuples", 
        _ => (),  
        TuplesQuickSort.quicksortByKey[Int](arr)(new Ordering[Int]{
          override def compare(a: Int, b: Int) = a - b
        }), 
        () => {}
     )
  }
}
