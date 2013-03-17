package miniboxing.benchmarks.ideal

import miniboxing.plugin.minispec

class List(val head: Int, val tail: List) {

  def length: Int = 1 + (if (tail != null) tail.length else 0)

  override def toString =
    head.toString + (if (tail != null) (", " + tail.toString) else "")

  def contains(e: Int): Boolean = {
    @annotation.tailrec def containsIntail(list: List, e: Int): Boolean =
      if (list.head == e)
        true
      else if (list.tail == null)
        false
      else
        containsIntail(list.tail, e)

    containsIntail(this, e)
  }

  override def hashCode(): Int = {
    @annotation.tailrec def tailHash(list: List, or: Int): Int = {
      val headhash = list.head.hashCode
      if (list.tail == null)
        headhash | or
      else
        tailHash(list.tail, or | headhash >> 8)
    }

    tailHash(this, 0)
  }

  def toString2: String = toString
}
