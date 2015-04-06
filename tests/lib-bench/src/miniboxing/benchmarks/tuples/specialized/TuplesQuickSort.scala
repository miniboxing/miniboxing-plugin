package miniboxing.benchmarks.tuples.specialized

object TuplesQuickSort {

  trait SpecializedOrdering[@specialized(Int) T] {
    def compare(t1: T, t2: T): Int
    def lt(t1: T, t2: T): Boolean = compare(t1, t2) < 0
    def gt(t1: T, t2: T): Boolean = compare(t1, t2) > 0
    def eq(t1: T, t2: T): Boolean = compare(t1, t2) == 0
  }

  object IntSpecializedOrdering extends SpecializedOrdering[Int] {
    override def compare(t1: Int, t2: Int): Int = t1 - t2
  }

  private def quicksort[@specialized(Int) T](a:Array[(T, Double)])(ord: SpecializedOrdering[T]): Array[(T, Double)] =  {
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

// specialization doesn't rewire the call to quicksort correctly:
//  def quicksortByKey[@specialized(Int) T](arr: Array[(T, Double)])(ord: SpecializedOrdering[T]): Array[(T, Double)] =
//    quicksort[T](arr)(ord)

  def quicksortByKey(arr: Array[(Int, Double)])(ord: SpecializedOrdering[Int]): Array[(Int, Double)] =
    quicksort[Int](arr)(ord)
}
