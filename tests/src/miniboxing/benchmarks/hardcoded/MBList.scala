
package miniboxing.benchmarks.hardcoded

import miniboxing.runtime.MiniboxConstants._
import miniboxing.runtime.MiniboxConversions._
import miniboxing.runtime.MiniboxTypeTagDispatch._

trait MBList[T] {
  // accessors
  def head: Long
  def head_J: Long
  def tail: MBList[T]
  def tail_J: MBList[T]
  // the getters are not specialized, as the constructor args are private[this]
  // methods are specialized
  def length: Int
  def length_J: Int
  override def toString: String
  def toString_J: String
  def contains(e: T): Boolean
  def contains_J(e: Long): Boolean
  override def hashCode(): Int
  def hashCode_J(): Int
  def toString2: String
  def toString2_J: String
  // <added for a quick test>
  def containsAny(e: Any): Boolean
  def containsAny_J(e: Any): Boolean
  // </added for a quick test>
}

class MBList_J[Tsp](_head: Long, _tail: MBList[Tsp], T_TypeTag: Byte) extends MBList[Tsp] {

  // head
  def head: Long = head_J
  def head_J: Long = _head

  // tail
  def tail: MBList[Tsp] = tail_J
  def tail_J: MBList[Tsp] = _tail

  // length
  def length: Int = length_J
  def length_J: Int = 1 + (if (tail != null) tail.length else 0)

  // toString
  override def toString = toString_J
  def toString_J = minibox2box[Tsp](head, T_TypeTag).toString + (if (tail != null) (", " + tail.toString_J) else "")

  // contains
  def contains(e: Tsp): Boolean = contains_J(box2minibox(e))
  def contains_J(e: Long): Boolean = {

    @annotation.tailrec def containsTail(list: MBList[Tsp]): Boolean =
      if (mboxed_eqeq(head, e))
        true
      else if (list.tail_J == null)
        false
      else
        containsTail(list.tail_J)

    containsTail(this)
  }

  // hashCode
  override def hashCode(): Int = hashCode_J()
  def hashCode_J(): Int = {

    @annotation.tailrec def tailHash(list: MBList[Tsp], or: Int): Int = {
      val headhash = mboxed_hashhash(list.head_J, T_TypeTag)
      if (list.tail_J == null)
        headhash | or
      else
        tailHash(list.tail_J, or | headhash >> 8)
    }

    tailHash(this, 0)
  }

  // toString2
  def toString2: String = toString2_J
  def toString2_J: String = toString_J

  // <added for a quick test>
  // contains: Any
  def containsAny(e: Any): Boolean = containsAny_J(e)
  def containsAny_J(e: Any): Boolean = {

    @annotation.tailrec def containsTail(list: MBList[Tsp]): Boolean =
      if (minibox2box(list.head, T_TypeTag) == e) // TODO this probably needs to be forwarded
        true
      else if (list.tail_J == null)
        false
      else
        containsTail(list.tail_J)

    containsTail(this)
  }
  // </added for a quick test>
}


// as if transformed by the classloader
class MBList_INT[Tsp](_head: Long, _tail: MBList[Tsp]) extends MBList[Tsp] {

  private[this] final val T_TypeTag: Byte = INT

  // head
  def head: Long = head_J
  def head_J: Long = _head

  // tail
  def tail: MBList[Tsp] = tail_J
  def tail_J: MBList[Tsp] = _tail

  // length
  def length: Int = length_J
  def length_J: Int = 1 + (if (tail != null) tail.length else 0)

  // toString
  override def toString = toString_J
  def toString_J = minibox2box[Tsp](head, T_TypeTag).toString + (if (tail != null) (", " + tail.toString_J) else "")

  // contains
  def contains(e: Tsp): Boolean = contains_J(box2minibox(e))
  def contains_J(e: Long): Boolean = {

    @annotation.tailrec def containsTail(list: MBList[Tsp]): Boolean =
      if (mboxed_eqeq(head, e))
        true
      else if (list.tail_J == null)
        false
      else
        containsTail(list.tail_J)

    containsTail(this)
  }

  // hashCode
  override def hashCode(): Int = hashCode_J()
  def hashCode_J(): Int = {

    @annotation.tailrec def tailHash(list: MBList[Tsp], or: Int): Int = {
      val headhash = mboxed_hashhash(list.head_J, T_TypeTag)
      if (list.tail_J == null)
        headhash | or
      else
        tailHash(list.tail_J, or | headhash >> 8)
    }

    tailHash(this, 0)
  }

  // toString2
  def toString2: String = toString2_J
  def toString2_J: String = toString_J

  // <added for a quick test>
  // contains: Any
  def containsAny(e: Any): Boolean = containsAny_J(e)
  def containsAny_J(e: Any): Boolean = {

    @annotation.tailrec def containsTail(list: MBList[Tsp]): Boolean =
      if (minibox2box(list.head, T_TypeTag) == e)
        true
      else if (list.tail_J == null)
        false
      else
        containsTail(list.tail_J)

    containsTail(this)
  }
  // </added for a quick test>
}
