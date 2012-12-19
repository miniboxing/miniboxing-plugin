package miniboxing.benchmarks

import miniboxing.plugin.minispec

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

  override def hashCode(): Int = {
    val headhash = head.##
    if (tail == null)
      headhash
    else
      (headhash >> 8 | tail.hashCode)
  }

  def toString2: String = toString
}
