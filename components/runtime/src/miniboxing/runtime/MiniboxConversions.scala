package miniboxing.runtime


object MiniboxConversions {

  // instead of importing, we copy the values here so the Scala compiler transforms the calls into tableswitches
  private[this] final val UNIT = 0;
  private[this] final val BOOLEAN = 1;
  private[this] final val BYTE = 2;
  private[this] final val SHORT = 3;
  private[this] final val CHAR = 4;
  private[this] final val INT = 5;
  private[this] final val LONG = 6;
  private[this] final val FLOAT = 7;
  private[this] final val DOUBLE = 8;
  private[this] final val REFERENCE = 9;

  /*
   * Conversions to and from Miniboxed representation and
   * the natural representation provided that we know the type
   * statically
   */
  @inline final def minibox2unit(l: Long): Unit = ()
  @inline final def minibox2boolean(l: Long): Boolean = (l != 0)
  @inline final def minibox2byte(l: Long): Byte = l.toByte
  @inline final def minibox2char(l: Long): Char = l.toChar
  @inline final def minibox2short(l: Long): Short = l.toShort
  @inline final def minibox2int(l: Long): Int = l.toInt
  @inline final def minibox2long(l: Long): Long = l.toLong
  @inline final def minibox2float(l: Long): Float =
    java.lang.Float.intBitsToFloat(l.toInt)
  @inline final def minibox2double(l: Long): Double =
    java.lang.Double.longBitsToDouble(l)

  @inline final def unit2minibox(u: Unit): Long = 0
  @inline final def boolean2minibox(b: Boolean): Long = if (b) 1 else 0
  @inline final def byte2minibox(b: Byte): Long = b.toLong
  @inline final def char2minibox(c: Char): Long = c.toLong
  @inline final def short2minibox(s: Short): Long = s.toLong
  @inline final def int2minibox(i: Int): Long = i.toLong
  @inline final def long2minibox(l: Long): Long = l
  @inline final def double2minibox(d: Double): Long =
    java.lang.Double.doubleToRawLongBits(d)
  @inline final def float2minibox(f: Float): Long =
    java.lang.Float.floatToRawIntBits(f).toLong


  /**
   * If code like the one below:
   *  class A[T](t: T) {
   *    override def toString = "A" + t
   *  }
   * in the specializer, we have to insert a box operation which takes into
   * account the type-tag of 't'. Normally, such an operation has to be
   * inserted during erasure, but we don't want to touch it.
   *
   * As a workaround, in our test examples we manually insert minibox2box
   * in such places.
   */
  final def minibox2box[T](l: Long, tag: Byte) : T = {
    val ret: Any = tag.asInstanceOf[Byte] match {
      // See https://issues.scala-lang.org/browse/SI-6956
      case UNIT => ()
      case BOOLEAN => (l != 0)
      case BYTE => l.toByte
      case CHAR => l.toChar
      case SHORT => l.toShort
      case INT => l.toInt
      case LONG => l
      case FLOAT => java.lang.Float.intBitsToFloat(l.toInt)
      case DOUBLE => java.lang.Double.longBitsToDouble(l)
      case REFERENCE => ???
    }
    ret.asInstanceOf[T]
  }

  /*
   *  We do not need to return the type tag also since it is known in the
   *  calling context.
   */
  final def box2minibox(a: Any): Long = a match {
    case b : Boolean => if (b) 1 else 0
    case b : Byte => b.toLong
    case u : Unit => 0
    case c : Char => c.toLong
    case s : Short => s.toLong
    case i : Int => i.toLong
    case l : Long => l
    case f : Float => java.lang.Float.floatToRawIntBits(f).toLong
    case d : Double => java.lang.Double.doubleToRawLongBits(d)
    case _ => sys.error("Trying to unbox a reference type")
  }

  /*
   *  Used in the rewiring, to keep the type and tag on all types
   */
  final def box2minibox_tt[T](a: T, tag: Byte): Long = a match {
    case b : Boolean => if (b) 1 else 0
    case b : Byte => b.toLong
    case u : Unit => 0
    case c : Char => c.toLong
    case s : Short => s.toLong
    case i : Int => i.toLong
    case l : Long => l
    case f : Float => java.lang.Float.floatToRawIntBits(f).toLong
    case d : Double => java.lang.Double.doubleToRawLongBits(d)
    case _ => sys.error("Trying to unbox a reference type")
  }
}
