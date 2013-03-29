package miniboxing.benchmarks.dispatcher

trait DispList[T] {
  // accessors
  def head: T
  def head_J(disp_m: Dispatcher[T]): Long
  def tail: DispList[T]
  def tail_J: DispList[T]
  // the getters are not specialized, as the constructor args are private[this]
  // methods are specialized
  def length: Int
  def length_J: Int
  override def toString: String
  def toString_J: String
  def contains(e: T): Boolean
  def contains_J(e: Long, disp_m: Dispatcher[T]): Boolean
  override def hashCode(): Int
  def hashCode_J(): Int
  def toString2: String
  def toString2_J: String
}

class DispList_J[Tsp](_head: Long, _tail: DispList[Tsp], disp: Dispatcher[Tsp]) extends DispList[Tsp] {

  // head
  def head: Tsp = disp.minibox2box(head_J(disp))
  def head_J(disp_m: Dispatcher[Tsp]): Long = _head

  // tail
  def tail: DispList[Tsp] = tail_J
  def tail_J: DispList[Tsp] = _tail

  // length
  def length: Int = length_J
  def length_J: Int = 1 + (if (tail != null) tail.length else 0)

  // toString
  override def toString = toString_J
  def toString_J = disp.minibox2box(head_J(disp)).toString + (if (tail != null) (", " + tail.toString_J) else "")

  // contains
  def contains(e: Tsp): Boolean = contains_J(disp.box2minibox(e), disp)
  def contains_J(e: Long, disp_m: Dispatcher[Tsp]): Boolean = {

    @annotation.tailrec def containsTail(list: DispList[Tsp], e: Long): Boolean =
      if (disp.mboxed_eqeq(list.head_J(disp), e))
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

    @annotation.tailrec def tailHash(list: DispList[Tsp], or: Int): Int = {
      val headhash = disp.mboxed_hashhash(list.head_J(disp))
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

class DispList_L[Tsp](_head: Tsp, _tail: DispList[Tsp]) extends DispList[Tsp] {

  // head
  def head: Tsp = _head
  def head_J(disp_m: Dispatcher[Tsp]): Long = disp_m.box2minibox(head)

  // tail
  def tail: DispList[Tsp] = _tail
  def tail_J: DispList[Tsp] = tail

  // length
  def length: Int = 1 + (if (tail != null) tail.length else 0)
  def length_J: Int = length

  // toString
  override def toString = toString_J
  def toString_J = head.toString + (if (tail != null) (", " + tail.toString_J) else "")

  // contains
  def contains(e: Tsp): Boolean = {
    @annotation.tailrec def containsTail(list: DispList[Tsp], e: Tsp): Boolean =
      if (list.head == e)
        true
      else if (list.tail == null)
        false
      else
        containsTail(list.tail, e)
    containsTail(this, e)
  }
  def contains_J(e: Long, disp_m: Dispatcher[Tsp]): Boolean = contains(disp_m.minibox2box(e))

  // hashCode
  override def hashCode: Int = {

    @annotation.tailrec def tailHash(list: DispList[Tsp], or: Int): Int = {
      val headhash = list.head.hashCode
      if (list.tail_J == null)
        headhash | or
      else
        tailHash(list.tail_J, or | headhash >> 8)
    }

    tailHash(this, 0)
  }
  def hashCode_J: Int = hashCode()

  // toString2
  def toString2: String = toString
  def toString2_J: String = toString2
}


/*
 * That's a lot of bytecode for constructing the class.
 * TODO: Can we factor out some common functionalty?
 */
abstract class DispListFactoryInterface {
  def newDispList_J[T$inst](_head: Long, _tail: DispList[T$inst], disp: Dispatcher[T$inst]): DispList[T$inst]
}

class DispListFactoryInstance_J extends DispListFactoryInterface {
  def newDispList_J[T$inst](_head: Long, _tail: DispList[T$inst], disp: Dispatcher[T$inst]): DispList[T$inst] =
    new DispList_J(_head, _tail, disp)
}

object DispListFactory {
  val fact = new Array[DispListFactoryInterface](10)

  @inline def newDispList_J[T$inst](_head: Long, _tail: DispList[T$inst], disp: Dispatcher[T$inst]): DispList[T$inst] =
    if (fact(disp.tag) == null)
      createFactoryAndObject(disp.tag)(_head, _tail, disp)
    else
      fact(disp.tag).newDispList_J(_head, _tail, disp)

  def createFactoryAndObject[T$inst](tag: Int)(_head: Long, _tail: DispList[T$inst], disp: Dispatcher[T$inst]): DispList[T$inst] =
    try {
      val classloader = miniboxing.classloader.MiniboxingClassLoader.classloader(DispListFactory.this)
      val clazz = classloader.findClass("miniboxing.benchmarks.dispatcher.DispListFactoryInstance_" + tag)
      val inst  = clazz.newInstance().asInstanceOf[DispListFactoryInterface]
      fact(tag) = inst
      fact(tag).newDispList_J(_head, _tail, disp)
    } catch {
      // TODO: What exactly do we want to catch?
      case other: Throwable =>
        fact(tag) = new DispListFactoryInstance_J()
        fact(tag).newDispList_J(_head, _tail, disp)
    }
}
