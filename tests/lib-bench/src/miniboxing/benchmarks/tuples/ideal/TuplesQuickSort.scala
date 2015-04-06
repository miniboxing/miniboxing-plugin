package miniboxing.benchmarks.tuples.ideal

object TuplesQuickSort {
  
  private def quicksort(a:Array[(Int, Double)])(implicit ord: Ordering[Int]): Array[(Int, Double)] =  {
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
  
  def quicksortByKey(arr: Array[(Int, Double)])(implicit ord: Ordering[Int]): Array[(Int, Double)] = 
    quicksort(arr)(ord)
}
