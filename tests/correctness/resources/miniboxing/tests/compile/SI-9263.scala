object TuplesQuickSort {

  private def quicksort[@specialized(Int) T](a:Array[(T, Double)]): Array[(T, Double)] = a
  def quicksortByKey[@specialized(Int) T](arr: Array[(T, Double)]): Array[(T, Double)] = quicksort[T](arr)
}
