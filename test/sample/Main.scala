package sample

import sample.scala.collection.immutable.List
import sample.scala.collection.mutable.ResizableArray
import sample.scala.collection.mutable.HashMap
import sample.scala.runtime.Tuple2Zipped



/**
 * Library sample with somehow complicated examples. 
 * It includes: List, ResizableArray, HashMap, Tuple2Zipped. 
 * 
 *  The parts are quite independent. 
 */
object Main {
  def println() = java.lang.System.out.println()
  def println(x: Any) = java.lang.System.out.println(x)
  
  def main(args: Array[String]) {
    val l:List[Int] = (List(1,2,3) ++ List(4,5,6))
    
    l map ((x:Int) => x + 1) foreach println
    println
    println(l exists (x => x == 2))
    println(l.reduceLeft[Int]({case (s, a) => s + a }) )
    
    val a = new ResizableArray[Int]()
    l foreach {a += _}
    a foreach println
    println
    
    val m = new HashMap[Int, Int]()
    l foreach {i => m(i) = i + 1}
    m foreach println
    println
    
    val t2z = new Tuple2Zipped(List[Int](1,2,3), List[Int](2,3,4))
    t2z.map({case (a,b) => a + ":" + b}) foreach println
  }
}
