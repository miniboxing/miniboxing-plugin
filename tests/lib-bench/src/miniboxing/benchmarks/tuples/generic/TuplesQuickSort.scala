package miniboxing.benchmarks.tuples.generic

object TuplesQuickSort {
  
  private def quicksort[T](a:Array[(T, Double)])(implicit ord: Ordering[T]): Array[(T, Double)] =  {
    def swap(i: Int, j: Int): Unit = {
      val t = a(i); a(i) = a(j); a(j) = t
    }
    def partition(l: Int, r: Int) {
      val pivot = a((l + r) / 2)
      var i = l; var j = r
      while (i <= j) {
        while (ord.lt(a(i)._1, pivot._1)) i += 1
        while (ord.gt(a(j)._1, pivot._1)) j -= 1
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
  }
  
  def quicksortByKey[T](arr: Array[(T, Double)])(implicit ord: Ordering[T]): Array[(T, Double)] =
    quicksort(arr)(ord)
}
