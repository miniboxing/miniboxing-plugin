package simple

class ArrayWrap[T : Manifest] {
  var array : Array[T] = _ 
  def newArray(t: T) = new Array[T](2) 
  def setElement(p: Int, t: T) = array(p) = t
  def getElement(p: Int) :T = array(p)
}