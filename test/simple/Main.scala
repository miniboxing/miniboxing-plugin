package test

object Main {
  def main(args: Array[String]) {
    var l : MBList_J = null
    for (i <- 1 to 100)
      l = new MBList_J(i, l, 5)
    println(l.length)
    println(l.toString)
  }
  
}