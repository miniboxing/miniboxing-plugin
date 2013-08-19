package miniboxing.benchmarks.minibox



class MBList[@miniboxed T](val head: T, val tail: MBList[T]) {

  def length: Int = 1 + (if (tail != null) tail.length else 0)

  override def toString =
    head.toString + (if (tail != null) (", " + tail.toString) else "")

  // no need for @tailrec, the tail calls transformation is smart enough
  private[this] final def containsTail(list: MBList[T], e: T): Boolean =
    if (list.head == e)
      true
    else if (list.tail == null)
      false
    else
      containsTail(list.tail, e)

  def contains(e: T): Boolean =
    containsTail(this, e)

  // no need for @tailrec, the tail calls transformation is smart enough
  private[this] final def tailHash(list: MBList[T], or: Int): Int = {
    val headhash = list.head.hashCode
    if (list.tail == null)
      headhash | or
    else
      tailHash(list.tail, or | headhash >> 8)
  }

  override def hashCode(): Int =
    tailHash(this, 0)

  def toString2: String = toString
}
