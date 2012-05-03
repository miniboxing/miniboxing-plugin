package simple

import runtime.MiniboxConversions._
import runtime.MiniboxTypeTagDispatch._
import runtime.MiniboxTypes._
import plugin.minispec 

class Cell[@minispec T : Manifest](t : T) {
  def toString2(): String = t.toString
  
  override def toString():String = {
    "Cell(" + minibox(t) + ")" + t.##
  }
  
  override def hashCode() = {
    println(toString())
    if (t.isInstanceOf[Int]){
      println("Int")
    }
    val a : Array[T] = new Array[T](1)
    a(0) = t
    val x : T = a(0)
    t.hashCode
  }
}

