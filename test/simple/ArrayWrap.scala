package simple

class ArrayWrap[T] {
  var array : Array[T] = _ 
  def newArray(t: T) = () // XXX : won't compile new Array[T](2) 
  def setElement(p: Int, t: T) = array(p) = t
  def getElement(p: Int) :T = array(p)
}