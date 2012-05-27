package test

import plugin.minispec

class MBList[@minispec T](head: T, tail: MBList[T]) {
  def length: Int = 1 + (if (tail != null) tail.length else 0)

  override def toString =
    head.toString + (if (tail != null) (", " + tail.toString) else "")

  def contains(e: T): Boolean =
    if (head == e)
      true
    else if (tail == null)
      false
    else
      tail.contains(e)
      
  def toString2 : String = toString
}

