package simple

import plugin.minispec 

@minispec
class Cell[@minispec T : Manifest](t : T) {
  def toString2(): String = t.toString
  
  override def toString():String = {
    "Cell(" + t + ")" + t.##
  }
  
  def meth() : Any = Cell.this.t
  override def hashCode() = {
    println(toString())
    if (t.isInstanceOf[Int]){
      println("Int")
    }
    val a : Array[T] = new Array[T](1)
    a(0) = t
    val x : T = a(0)
    var x1 : Any = a(0)
    x1 = a(1)
    val y : Cell[_] = new Cell[T](x1.asInstanceOf[T])
    // Why is it expanded to: 
    // val y: simple.Cell[_] = new simple.Cell[T](x1.asInstanceOf[T])(Cell.this.evidence$1)(Cell.this.evidence$1);
    t.hashCode
  }
}

