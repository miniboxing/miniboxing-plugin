package benchmarks

object Benchmark {
  def timed(msg: String, f: => Unit, iter: Int) = {
    f; f; f 
    System.gc
    
    
//    println("*****************************************************")
    val tb = System.nanoTime
    var i = 0
    while (i < iter) {
      f
      i += 1
    }
    System.gc
    println(msg + " -- " + (System.nanoTime - tb)/ 1000000d)
  }
}