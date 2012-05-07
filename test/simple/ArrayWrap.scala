package simple

import plugin.minispec

trait Array_intf[T] {
  
  def array : Array[T]
  def newArray_J(t:T) : Array[T]
}

class ArrayWrap[@minispec T : Manifest] {
  var array : Array[T] = _ 
  def newArray(t: T) = new Array[T](2) 
  def setElement(p: Int, t: T) = array(p) = t
  def getElement(p: Int) :T = array(p)
}

