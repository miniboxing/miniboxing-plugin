package miniboxing.benchmarks.generic

/**
 * The arrays are tricky since we need to use the natural representation for them.
 *
 * The plugin if arrays are used inside minispeced code according to the following rules:
 *  - every array creation is done via: MiniboxArray.newArray[T](len)
 *  - every access to the array requires a cast to its type: array(p)
 *  - local array variables are not supported
 */
class ResizableArray[T: Manifest] {
  final val initialSize = 4
  var size: Int = initialSize
  var elemCount: Int = 0
  var array: Array[T] = new Array[T](initialSize)
  var newarray: Array[T] = _

  def extend(): Unit = {
    if (elemCount == size) {
      var pos = 0
      newarray = new Array[T](2 * size)
      while(pos < size) {
        newarray(pos) = array(pos)
        pos += 1
      }
      array = newarray
      size *= 2
    }
  }

  def add(elem: T) = {
    extend()
    array(elemCount) = elem
    elemCount += 1
  }


  def reverse() = {
    var pos = 0
    while (pos * 2 < elemCount) {
      val tmp1: T = getElement(pos)
      val tmp2: T = getElement(elemCount-pos-1)
      setElement(pos, tmp2)
      setElement(elemCount-pos-1, tmp1)
      pos += 1
    }
  }

  def contains(elem: T): Boolean = {
    var pos = 0
    while (pos < elemCount){
      if (getElement(pos) == elem)
        return true
      pos += 1
    }
    return false
  }

  def length = elemCount

  @inline final def setElement(p: Int, t: T) = {
    array(p) = t
  }
  @inline final def getElement(p: Int): T = array(p)
}

