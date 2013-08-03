package miniboxing.benchmarks.hardcoded.fullswitch

import miniboxing.runtime.MiniboxConstants._
import miniboxing.runtime.MiniboxConversions._
import miniboxing.runtime.MiniboxTypeTagDispatch._
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

/*
 * That's a lot of bytecode for constructing the class.
 * TODO: Can we factor out some common functionalty?
 */
abstract class MBListFactoryInterface {
  def newMBList_J[T$inst](_head: Long, _tail: MBList[T$inst], T_TypeTag: Byte): MBList[T$inst]
}

class MBListFactoryInstance_class_J extends MBListFactoryInterface {
  def newMBList_J[T$inst](_head: Long, _tail: MBList[T$inst], T_TypeTag: Byte): MBList[T$inst] =
    new MBList_class_J(_head, _tail, T_TypeTag)
}

object MBListFactory {

  val fact = new Array[MBListFactoryInterface](10)

  @inline def newMBList_J[T$inst](_head: Long, _tail: MBList[T$inst], T_TypeTag: Byte): MBList[T$inst] = {
    if (fact(T_TypeTag) == null)
      createFactoryAndObject(T_TypeTag)(_head, _tail, T_TypeTag)
    else
      fact(T_TypeTag).newMBList_J(_head, _tail, T_TypeTag)
  }

  def createFactoryAndObject[T$inst](tag: Int)(_head: Long, _tail: MBList[T$inst], T_TypeTag: Byte): MBList[T$inst] =
    try {
      val classloader = miniboxing.classloader.MiniboxingClassLoader.classloader(MBListFactory.this)
      val clazz = classloader.findClass("miniboxing.benchmarks.hardcoded.fullswitch.MBListFactoryInstance_class_" + tag)
      val inst  = clazz.newInstance().asInstanceOf[MBListFactoryInterface]
      fact(tag) = inst
      fact(tag).newMBList_J(_head, _tail, T_TypeTag)
    } catch {
      // TODO: What exactly do we want to catch?
      case other: Throwable =>
        fact(tag) = new MBListFactoryInstance_class_J()
        fact(tag).newMBList_J(_head, _tail, T_TypeTag)
    }
}
