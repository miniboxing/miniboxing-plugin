package miniboxing.benchmarks.tuples.launch.tests

import scala.util.Random

import miniboxing.benchmarks.tuples.miniboxed.hardcoded.TuplesQuickSort
import miniboxing.runtime.MiniboxConstants
import miniboxing.runtime.MiniboxConversions
import miniboxing.runtime.MiniboxConversionsDouble
import miniboxing.runtime.MiniboxedTuple

trait HardcodedMiniboxedBenchTest extends BaseTest {
  
  def testHardcodedMiniboxed() = {
    var arr: Array[(Int, Double)] = new Array[(Int, Double)](testSize)
    val random = new Random(0)
    for( ind <- 0 to (testSize - 1)){
      arr(ind) = MiniboxedTuple.newTuple2_long_double[Int, Double](
        MiniboxConstants.INT,
        MiniboxConstants.DOUBLE,
        MiniboxConversions.int2minibox(random.nextInt(testSize)),
        MiniboxConversionsDouble.double2minibox(0.0)
      )
    }

    test(
      "miniboxed_hardcoded-" + arr.length, 
      "quicksort_array_of_tuples", 
      _ => (),  
      TuplesQuickSort.quicksortByKeyLong[Int](arr)(TuplesQuickSort.IntMiniboxedOrdering), 
      () => {}
    )
  }
}
