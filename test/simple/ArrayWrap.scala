package simple

import plugin.minispec

/*
trait Array_intf[T] {
  
  def array : Array[T]
  def newArray_J(t:T) : Array[T]
}
*/

class ArrayWrap[@minispec T : Manifest] {
  private var array : MiniboxArray = _ 
  def newArray(len: Int): Unit = array = MiniboxArray[T](len) 
  def setElement(p: Int, t: T) = {
    array(i) = t
  }
  def getElement(p: Int): T = array(p)
}

