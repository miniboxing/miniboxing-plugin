package sample.scala.collection.mutable

import sample.scala.collection.generic.Growable
import sample.scala.collection.generic.Shrinkable

// XXX : minispec array
class ResizableArray[A] extends Growable[A] with Shrinkable[A] {
  protected def initialSize: Int = 2
  protected var array: Array[AnyRef] = new Array[AnyRef](math.max(initialSize, 1))
  protected var size0: Int = 0

  override def clear() = reduceToSize(0)
  
  override def --() = {
    reduceToSize(size0-1)
    size0 -= 1;
    this
  }
  
  override def +=(elem : A) = {
    ensureSize(size0+1)
    size0 += 1
    this(size0-1) = elem
    this
  }
  
  def length: Int = size0

  def apply(idx: Int) = {
    if (idx >= size0) throw new IndexOutOfBoundsException(idx.toString)
    array(idx).asInstanceOf[A]
  }

  def update(idx: Int, elem: A) {
    if (idx >= size0) throw new IndexOutOfBoundsException(idx.toString)
    array(idx) = elem.asInstanceOf[AnyRef]
  }

  def foreach[U](f: A => U) {
    var i = 0
    val top = size0
    while (i < top) {
      f(array(i).asInstanceOf[A])
      i += 1
    }
  }

  def reduceToSize(sz: Int) {
    while (size0 > sz) {
      size0 -= 1
      array(size0) = null
    }
  }
  protected def ensureSize(n: Int) {
    if (n > array.length) {
      var newsize = array.length * 2
      while (n > newsize)
        newsize = newsize * 2

      val newar: Array[AnyRef] = new Array(newsize)
      compat.Platform.arraycopy(array, 0, newar, 0, size0)
      array = newar
    }
  }

  /** Swap two elements of this array.
   */
  protected def swap(a: Int, b: Int) {
    val h = array(a)
    array(a) = array(b)
    array(b) = h
  }

  /** Move parts of the array.
   */
  protected def copy(m: Int, n: Int, len: Int) {
    compat.Platform.arraycopy(array, m, array, n, len)
  }
}
