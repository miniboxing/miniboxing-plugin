package miniboxing.benchmarks.tuples.ideal

object TuplesQuickSort {
  
  trait IdealOrdering {
    def compare(t1: Int, t2: Int): Int
    def lt(t1: Int, t2: Int): Boolean = compare(t1, t2) < 0
    def gt(t1: Int, t2: Int): Boolean = compare(t1, t2) > 0
    def eq(t1: Int, t2: Int): Boolean = compare(t1, t2) == 0
  }
  
  object IntIdealOrdering extends IdealOrdering {
    override def compare(t1: Int, t2: Int): Int = t1 - t2
  }
  
  private def quicksort(a:Array[(Int, Double)])(ord: IdealOrdering): Array[(Int, Double)] =  {
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
  
  def quicksortByKey(arr: Array[(Int, Double)])(ord: IdealOrdering): Array[(Int, Double)] = 
    quicksort(arr)(ord)
}
