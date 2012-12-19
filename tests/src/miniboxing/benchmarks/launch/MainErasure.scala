package miniboxing.benchmarks
package erasure

object MainErasure {
  val N = 1000000
  val M = 20

  def list_insert(): MBList[Int] = {
    var l: MBList[Int] = null
    var i = 0
    while (i < N) {
      l = new MBList[Int](i, l)
      i += 1
    }
    l
  }

  def list_hashCode(list: MBList[Int]): Int = {
    list.hashCode
  }

  def list_find(l: MBList[Int]) = {
    var i = 0
    while (i < N) {
      l contains i
      i += 10000
    }
  }

  def array_insert(): ResizableArray[Int] = {
    val a: ResizableArray[Int] = new ResizableArray[Int]
    var i = 0
    while (i < N) {
      a add i
      i += 1
    }
    a
  }

  def array_reverse(a: ResizableArray[Int]) = {
    a.reverse
  }

  def array_find(a: ResizableArray[Int]) = {
    var i = 0
    while (i < N) {
      a contains i
      i += 10000
    }
  }

  def main(args: Array[String]) {
    val a = array_insert()
    Benchmark.timed("array insert  ", array_insert(), 30)
    Benchmark.timed("array_reverse ", array_reverse(a), 75)
    Benchmark.timed("array_find    ", array_find(a), 3)

    val l = list_insert()
    Benchmark.timed("list_insert   ", list_insert(), 30)
    Benchmark.timed("list_hashCode ", list_hashCode(l), 50)
    Benchmark.timed("list_find     ", list_find(l), 1)

  }
}
