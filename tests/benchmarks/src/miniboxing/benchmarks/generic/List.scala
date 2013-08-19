package miniboxing.benchmarks.generic



class List[T](val head: T, val tail: List[T]) {

  def length: Int = 1 + (if (tail != null) tail.length else 0)

  override def toString =
    head.toString + (if (tail != null) (", " + tail.toString) else "")

  def contains(e: T): Boolean = {
    @annotation.tailrec def containsTail(list: List[T]): Boolean =
      if (list.head == e)
        true
      else if (list.tail == null)
        false
      else
        containsTail(list.tail)

    containsTail(this)
  }

  override def hashCode(): Int = {
    @annotation.tailrec def tailHash(list: List[T], or: Int): Int = {
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
