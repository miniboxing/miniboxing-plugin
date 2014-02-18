package miniboxing.tests.compile.separate

/**
 * The arrays are tricky since we need to use the natural representation for them.
 *
 * The plugin if arrays are used inside minispeced code according to the following rules:
 *  - every array creation is done via: MiniboxArray.newArray[T](len)
 *  - every access to the array requires a cast to its type: array.asInstanceOf[Array[T]](p)
 *  - local array variables are not supported
 */
class MBResizableArray[@miniboxed T](mf: Manifest[T])  {
  final val initialSize = 4
  var size: Int = initialSize
  var elemCount: Int = 0
  var array: Array[T] = mf.newArray(initialSize)
  var newarray: Array[T] = _

  def extend(): Unit = {
    if (elemCount == size) {
      var pos = 0
      newarray = mf.newArray(2 * size)
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

  def length: Int = elemCount

  @inline final def setElement(p: Int, t: T) = {
    array(p) = t
  }
  @inline final def getElement(p: Int): T = array(p)

  override def toString() = {
    var pos = 0
    var ret = ""
    while (pos < elemCount) {
      ret += getElement(pos).toString + ", "
      pos  += 1
    }
    ret
  }
}


