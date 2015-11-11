package miniboxing.benchmarks.hardcoded.java_rt_1

import miniboxing.internal.MiniboxConstants._
import miniboxing.internal.MiniboxConversions._
import miniboxing.internal.MiniboxDispatch._
import miniboxing.classloader.MiniboxingClassLoader

trait MBList[T] {
  // accessors
  def head: T
  def head_J(T_Type_m: Byte): Long
  def tail: MBList[T]
  def tail_J: MBList[T]
  // the getters are not specialized, as the constructor args are private[this]
  // methods are specialized
  def length: Int
  def length_J: Int
  override def toString: String
  def toString_J: String
  def contains(e: T): Boolean
  def contains_J(e: Long, T_Type_m: Byte): Boolean
  override def hashCode(): Int
  def hashCode_J(): Int
  def toString2: String
  def toString2_J: String
}

class MBList_class_J[Tsp](_head: Long, _tail: MBList[Tsp], T_TypeTag: Byte) extends MBList[Tsp] {

  // head
  def head: Tsp = minibox2box[Tsp](head_J(T_TypeTag), T_TypeTag)
  def head_J(T_Type_m: Byte): Long = _head

  // tail
  def tail: MBList[Tsp] = tail_J
  def tail_J: MBList[Tsp] = _tail

  // length
  def length: Int = length_J
  def length_J: Int = 1 + (if (tail != null) tail.length else 0)

  // toString
  override def toString = toString_J
  def toString_J = minibox2box[Tsp](head_J(T_TypeTag), T_TypeTag).toString + (if (tail != null) (", " + tail.toString_J) else "")

  // contains
  def contains(e: Tsp): Boolean = contains_J(box2minibox(e), T_TypeTag)
  def contains_J(e: Long, T_Type_m: Byte): Boolean = {

    @annotation.tailrec def containsTail(list: MBList[Tsp], e: Long): Boolean =
      if (mboxed_eqeq(list.head_J(T_TypeTag), e))
        true
      else if (list.tail_J == null)
        false
      else
        containsTail(list.tail_J, e)

    containsTail(this, e)
  }

  // hashCode
  override def hashCode(): Int = hashCode_J()
  def hashCode_J(): Int = {

    @annotation.tailrec def tailHash(list: MBList[Tsp], or: Int): Int = {
      val headhash = mboxed_hashCode(list.head_J(T_TypeTag), T_TypeTag)
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
}


class MBList_L[Tsp](_head: Tsp, _tail: MBList[Tsp]) extends MBList[Tsp] {

  // head
  def head: Tsp = _head
  def head_J(T_Type_m: Byte): Long = box2minibox(head)

  // tail
  def tail: MBList[Tsp] = _tail
  def tail_J: MBList[Tsp] = tail

  // length
  def length: Int = 1 + (if (tail != null) tail.length else 0)
  def length_J: Int = length

  // toString
  override def toString = _head.toString + (if (tail != null) (", " + tail.toString) else "")
  def toString_J = toString

  // contains
  def contains(e: Tsp): Boolean = {

    @annotation.tailrec def containsTail(list: MBList[Tsp], e: Tsp): Boolean =
      if (list.head == e)
        true
      else if (list.tail == null)
        false
      else
        containsTail(list.tail, e)

    containsTail(this, e)
  }
  def contains_J(e: Long, T_Type_m: Byte): Boolean = contains(minibox2box(e, T_Type_m))
  // hashCode
  override def hashCode(): Int = {

    @annotation.tailrec def tailHash(list: MBList[Tsp], or: Int): Int = {
      val headhash = list.head.hashCode
      if (list.tail == null)
        headhash | or
      else
        tailHash(list.tail, or | headhash >> 8)
    }

    tailHash(this, 0)
  }
  def hashCode_J(): Int = hashCode()
  // toString2
  def toString2: String = toString
  def toString2_J: String = toString2
}