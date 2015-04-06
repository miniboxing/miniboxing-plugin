package miniboxing.benchmarks.tuples.specialized

object TuplesQuickSort {
  
  private def quicksort[@specialized T](a:Array[(T, Double)])(implicit ord: Ordering[T]): Array[(T, Double)] =  {
    def swap(i: Int, j: Int) {
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
//    if (a.length < 2) a
//    else {
//      val pivot = a(a.length / 2)
//      quicksort (a.filter(p => ord.lt(p._1, pivot._1))) ++ (a.filter(p => ord.equiv(p._1, pivot._1))) ++
//        quicksort (a.filter(p => ord.gt(p._1, pivot._1)))
//    }
  }
  
  def quicksortByKey[@specialized T](arr: Array[(T, Double)])(implicit ord: Ordering[T]): Array[(T, Double)] =
    quicksort(arr)(ord)
}
