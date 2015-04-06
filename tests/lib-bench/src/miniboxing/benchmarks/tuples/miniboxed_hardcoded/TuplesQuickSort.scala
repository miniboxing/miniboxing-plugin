package miniboxing.benchmarks.tuples.miniboxed_hardcoded

import miniboxing.runtime.MiniboxConstants
import miniboxing.runtime.MiniboxedTuple
import miniboxing.runtime.MiniboxConversions

object TuplesQuickSort {
  
  private class MiniboxHardcodedIntOrdering extends Ordering[Long] {
     override def compare(x: Long, y: Long) = MiniboxConversions.minibox2int(x) - MiniboxConversions.minibox2int(y)
  }

  private def quicksort[T](a: Array[(T, Double)]): Array[(T, Double)] = {
    def swap(i: Int, j: Int) {
      val t = a(i); a(i) = a(j); a(j) = t
    }
    def partition(l: Int, r: Int) {
      val ord = new MiniboxHardcodedIntOrdering()
      val pivot = a((l + r) / 2)
      var i = l; var j = r
      while (i <= j) {
        while (ord.lt(MiniboxedTuple.tuple2_accessor_1_long(MiniboxConstants.INT, a(i)), MiniboxedTuple.tuple2_accessor_1_long(MiniboxConstants.INT, pivot))) i += 1
        while (ord.gt(MiniboxedTuple.tuple2_accessor_1_long(MiniboxConstants.INT, a(j)), MiniboxedTuple.tuple2_accessor_1_long(MiniboxConstants.INT, pivot))) j -= 1
        if (i <= j) {
          swap(i, j)
          i += 1
          j -= 1
        }
      }
      if (l < j) partition(l, j)
      if (j < r) partition(i, r)
    }
    partition(0, a.length - 1)
    a
    
    
//    
//    if (a.length < 2) a
//    else {
//      val pivot = a(a.length / 2)
//      quicksort(a.filter(p => ord.lt(MiniboxedTuple.tuple2_accessor_1_long(MiniboxConstants.INT, p), MiniboxedTuple.tuple2_accessor_1_long(MiniboxConstants.INT, pivot)))) ++
//        (a.filter(p => ord.equiv(MiniboxedTuple.tuple2_accessor_1_long(MiniboxConstants.INT, p), MiniboxedTuple.tuple2_accessor_1_long(MiniboxConstants.INT, pivot)))) ++
//        quicksort(a.filter(p => ord.gt(MiniboxedTuple.tuple2_accessor_1_long(MiniboxConstants.INT, p), MiniboxedTuple.tuple2_accessor_1_long(MiniboxConstants.INT, pivot))))
//    }
  }
  
  def quicksortByKey[T](arr: Array[(T, Double)]): Array[(T, Double)] =
    quicksort(arr)
}
