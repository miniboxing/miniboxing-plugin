package miniboxing.benchmarks.dispatcher

trait DispList[T] {
  // accessors
  def head: T
  def head_J: Long
  def tail: DispList[T]
  def tail_J: DispList[T]
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

class DispList_J[Tsp](_head: Long, _tail: DispList[Tsp], disp: Dispatcher[Tsp]) extends DispList[Tsp] {

  // head
  def head: Tsp = disp.minibox2box(head_J)
  def head_J: Long = _head

  // tail
  def tail: DispList[Tsp] = tail_J
  def tail_J: DispList[Tsp] = _tail

  // length
  def length: Int = length_J
  def length_J: Int = 1 + (if (tail != null) tail.length else 0)

  // toString
  override def toString = toString_J
  def toString_J = disp.minibox2box(head_J).toString + (if (tail != null) (", " + tail.toString_J) else "")

  // contains
  def contains(e: Tsp): Boolean = contains_J(disp.box2minibox(e))
  def contains_J(e: Long): Boolean = {

    @annotation.tailrec def containsTail(list: DispList[Tsp]): Boolean =
      if (disp.mboxed_eqeq(list.head_J, e))
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

    @annotation.tailrec def tailHash(list: DispList[Tsp], or: Int): Int = {
      val headhash = disp.mboxed_hashhash(list.head_J)
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

    @annotation.tailrec def containsTail(list: DispList[Tsp]): Boolean =
      if (disp.minibox2box(list.head_J) == e) // TODO this probably needs to be forwarded
        true
      else if (list.tail_J == null)
        false
      else
        containsTail(list.tail_J)

    containsTail(this)
  }
  // </added for a quick test>
}

/*
 * That's a lot of bytecode for constructing the class.
 * TODO: Can we factor out some common functionalty?
 */
trait DispListFactoryInterface {
  def newDispList_J[T$inst](_head: Long, _tail: DispList[T$inst], disp: Dispatcher[T$inst]): DispList[T$inst]
}

class DispListFactoryInstance_J extends DispListFactoryInterface {
  def newDispList_J[T$inst](_head: Long, _tail: DispList[T$inst], disp: Dispatcher[T$inst]): DispList[T$inst] =
    new DispList_J(_head, _tail, disp)
}

object DispListFactory {
  val factories = new Array[DispListFactoryInterface](10)

  def newDispList_J[T$inst](_head: Long, _tail: DispList[T$inst], disp: Dispatcher[T$inst]): DispList[T$inst] = {
    val tag = disp.tag
    try {
      val fact = factories(tag)
      fact.newDispList_J(_head, _tail, disp)
    } catch {
      // factory creation is outside the critical path
      case _: NullPointerException =>
        try {
          val classloader = miniboxing.classloader.MiniboxingClassLoader.classloader(DispListFactory.this)
          val clazz = classloader.findClass("miniboxing.benchmarks.dispatcher.DispListFactoryInstance_" + tag)
          val inst  = clazz.newInstance().asInstanceOf[DispListFactoryInterface]
          factories(tag) = inst
          newDispList_J(_head, _tail, disp)
        } catch {
//          case cnf: ClassNotFoundException =>
//            new DispList_J[T$inst](_head, _tail, disp)
          case other: Throwable => throw other
        }
    }
  }
}
