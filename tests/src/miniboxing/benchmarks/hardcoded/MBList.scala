package miniboxing.benchmarks.hardcoded
import miniboxing.runtime.MiniboxConversions._
import miniboxing.runtime.MiniboxTypeTagDispatch._

trait MBList[T] {
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

class MBList_J[Tsp](head: Long, tail: MBList_J[Tsp], T_TypeTag: Byte) extends MBList[Tsp] {
  // length
  def length: Int = length_J
  def length_J: Int = 1 + (if (tail != null) tail.length else 0)

  // toString
  override def toString = toString_J
  def toString_J = minibox2box[Tsp](head, T_TypeTag).toString + (if (tail != null) (", " + tail.toString_J) else "")

  // contains
  def contains(e: Tsp): Boolean = contains_J(box2minibox(e))
  def contains_J(e: Long): Boolean =
    if (mboxed_eqeq(head, T_TypeTag, e, T_TypeTag))
      true
    else if (tail == null)
      false
    else
      tail.contains_J(e)

  // hashCode
  override def hashCode(): Int = hashCode_J()
  def hashCode_J(): Int = {
    val headhash = mboxed_hashhash(head, T_TypeTag)
    if (tail == null)
      headhash
    else
      (headhash >> 8 | tail.hashCode_J)
  }

  // toString2
  def toString2: String = toString2_J
  def toString2_J: String = toString_J

  // <added for a quick test>
  // contains: Any
  def containsAny(e: Any): Boolean = containsAny_J(e)
  def containsAny_J(e: Any): Boolean =
  if (minibox2box(head, T_TypeTag) == e)
    true
    else if (tail == null)
      false
      else
        tail.containsAny_J(e)
  // </added for a quick test>
}
