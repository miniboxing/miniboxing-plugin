package miniboxing.example

import MbArray._

object Test {
  final val len = 10000000
  final val steps = 10
 
  def sumA(ary: Array[Any]) = {
    var i = 0
    while (i < len) {
      ary(i) = 1
      ary(0) = ary(0).asInstanceOf[Int] + ary(i).asInstanceOf[Int]
      i += 1
    }
    ary(0)
  }
 
  def sumB(ary: MbArray[Int]) = {
    var i = 0
    while (i < len) {
      ary(i) = 1
      ary(0) = ary(0) + ary(i)
      i += 1
    }
    ary(0)
  }

  def sumC(ary: Array[Int]) = {
    var i = 0
    while (i < len) {
      ary(i) = 1
      ary(0) = ary(0) + ary(i)
      i += 1
    }
    ary(0)
  }
 
 
  def main(args: Array[String]): Unit = {
    val a = new Array[Any](len)
    val b = MbArray.empty[Int](len)
    val c = new Array[Int](len)
    var ta = 0l
    var tb = 0l
    var tc = 0l
   
    println(a.getClass())
    for (i <- 0 until steps) {
      val startA = System.nanoTime
      val sA = sumA(a)
      // println(sA + " in " + (System.nanoTime - startA) / 1000000.0 + " milliseconds")
      ta += System.nanoTime - startA
    }
  
    println(b.getClass())
    for (i <- 0 until steps) {
      val startB = System.nanoTime
      val sB = sumB(b)
      // println(sB + " in " + (System.nanoTime - startB) / 1000000.0 + " milliseconds")
      tb += System.nanoTime - startB
    }
   
    println(c.getClass())
    for (i <- 0 until steps) {
      val startC = System.nanoTime
      val sC = sumC(c)
      // println(sC + " in " + (System.nanoTime - startC) / 1000000.0 + " milliseconds")
      tc += System.nanoTime - startC
    }

    assert(ta > tb, s"Array[Any] ($ta) is worse than MbArray[Int] ($tb)")
    // may not always be the case:
    // assert(tb > tc, s"MbArray[Int] ($tb) is worse than Array[Int] ($tc)")
  }
}
