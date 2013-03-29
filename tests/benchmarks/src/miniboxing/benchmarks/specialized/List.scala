package miniboxing.benchmarks.specialized

class List[@specialized T](val head: T, val tail: List[T]) {

  def length(e: T): Int = 1 + (if (tail != null) tail.length(e) else 0)

  def toString(e: T): String =
    head.toString + (if (tail != null) (", " + tail.toString(e)) else "")

//  this was for the generic signature:
//    def contains(e: Any) which was awfully slow
//  but now we use the specific signature on both sides
//  TODO: Also use the generic signature for both
//
//  // FIXME: This is what specialization is forcing on us:
//  // we can't put the containsTail method inside the contains method as it won't get specialized at all.
//  // so we resort to manually lifting it top level in the class
//  @annotation.tailrec private[this] final def containsTail(list: List[T], e: T): Boolean =
//    if (list.head == e)
//      true
//    else if (list.tail == null)
//      false
//    else
//      containsTail(list.tail, e)
//
//  def contains(e: Any): Boolean =
//    containsTail(this, e)

  def contains(e: T): Boolean = {
    @annotation.tailrec def containsTail(list: List[T], e: T): Boolean =
      if (list.head == e)
        true
      else if (list.tail == null)
        false
      else
        containsTail(list.tail, e)

    containsTail(this, e)
  }

  def hashCode(e: T): Int = {
    @annotation.tailrec def tailHash(list: List[T], or: Int): Int = {
      val headhash = list.head.hashCode
      if (list.tail == null)
        headhash | or
      else
        tailHash(list.tail, or | headhash >> 8)
    }

    tailHash(this, 0)
  }

  def toString2(e: T): String = toString
}

