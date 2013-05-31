package miniboxing.runtime


object MiniboxConversions {
  import MiniboxTypes._

  /*
   * Conversions to and from Miniboxed representation and
   * the natural representation provided that we know the type
   * statically
   */
  @inline final def MiniboxToUnit(l: Long): Unit = ()
  @inline final def MiniboxToBoolean(l: Long): Boolean = (l != 0)
  @inline final def MiniboxToByte(l: Long): Byte = l.toByte
  @inline final def MiniboxToChar(l: Long): Char = l.toChar
  @inline final def MiniboxToShort(l: Long): Short = l.toShort
  @inline final def MiniboxToInt(l: Long): Int = l.toInt
  @inline final def MiniboxToLong(l: Long): Long = l.toLong
  @inline final def MiniboxToFloat(l: Long): Float =
    java.lang.Float.intBitsToFloat(l.toInt)
  @inline final def MiniboxToDouble(l: Long): Double =
    java.lang.Double.longBitsToDouble(l)

  @inline final def UnitToMinibox(u: Unit): Long = 0
  @inline final def BooleanToMinibox(b: Boolean): Long = if (b) 1 else 0
  @inline final def ByteToMinibox(b: Byte): Long = b.toLong
  @inline final def CharToMinibox(c: Char): Long = c.toLong
  @inline final def ShortToMinibox(s: Short): Long = s.toLong
  @inline final def IntToMinibox(i: Int): Long = i.toLong
  @inline final def LongToMinibox(l: Long): Long = l
  @inline final def DoubleToMinibox(d: Double): Long =
    java.lang.Double.doubleToRawLongBits(d)
  @inline final def FloatToMinibox(f: Float): Long =
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
  @inline final def minibox2box[T](l: Long, tag: Tag) : T = {
    val ret: Any = tag.asInstanceOf[Byte] match {
      // See https://issues.scala-lang.org/browse/SI-6956
      case 0 /* UNIT */ => ()
      case 1 /* BOOLEAN */ => (l != 0)
      case 2 /* BYTE */ => l.toByte
      case 3 /* CHAR */ => l.toChar
      case 4 /* SHORT */ => l.toShort
      case 5 /* INT */ => l.toInt
      case 6 /* LONG */ => l
      case 7 /* FLOAT */ => java.lang.Float.intBitsToFloat(l.toInt)
      case 8 /* DOUBLE */ => java.lang.Double.longBitsToDouble(l)
      case 9 /* REFERENCE */ => ???
    }
    ret.asInstanceOf[T]
  }

  /*
   *  We do not need to return the type tag also since it is known in the
   *  calling context.
   */
  @inline final def box2minibox(a: Any): Long = a match {
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
