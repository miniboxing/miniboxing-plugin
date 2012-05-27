package test

import runtime._

object Main {
  def main(args: Array[String]) {
    var l : MBList_J = null
    for (i <- 1 to 100)
      l = new MBList_J(i, l, MiniboxConstants.INT)
    println(l.length)
    println(l.toString)
    println(l.contains(10))
  }
  
}