package miniboxing.test

import miniboxing.plugin.minispec
import miniboxing.runtime.MiniboxArray

/**
 * The arrays are tricky since we need to use the natural representation for them.
 *
 * The plugin if arrays are used inside minispeced code according to the following rules:
 *  - every array creation is done via: MiniboxArray.newArray[T](len)
 *  - every access to the array requires a cast to its type: array.asInstanceOf[Array[T]](p)
 */
class ArrayWrap[@minispec T] {
  private var array : Array[T] = _
  def newArray(len: Int): Unit = array = MiniboxArray.newArray[T](len)
  def setElement(p: Int, t: T) = {
    array.asInstanceOf[Array[T]](p) = t
  }
  def getElement(p: Int): T = array.asInstanceOf[Array[T]](p)
}

