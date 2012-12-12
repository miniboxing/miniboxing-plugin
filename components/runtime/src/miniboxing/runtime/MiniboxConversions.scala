package miniboxing.runtime


object MiniboxConversions {
  import MiniboxTypes._
  import MiniboxConstants._
  
  /*
   * Conversions to and from Miniboxed representation and 
   * the natural representation provided that we know the type
   * statically
   */
  @inline final def MiniboxToUnit(l: Minibox): Unit = ()
  @inline final def MiniboxToBoolean(l: Minibox): Boolean = (l != 0)
  @inline final def MiniboxToByte(l: Minibox): Byte = l.toByte
  @inline final def MiniboxToChar(l: Minibox): Char = l.toChar
  @inline final def MiniboxToShort(l: Minibox): Short = l.toShort
  @inline final def MiniboxToInt(l: Minibox): Int = l.toInt
  @inline final def MiniboxToLong(l: Minibox): Long = l.toLong
  @inline final def MiniboxToFloat(l: Minibox): Float = 
    java.lang.Float.intBitsToFloat(l.toInt)
  @inline final def MiniboxToDouble(l: Minibox): Double = 
    java.lang.Double.longBitsToDouble(l)
  
  @inline final def UnitToMinibox(u: Unit): Minibox = 0
  @inline final def BooleanToMinibox(b: Boolean): Minibox = if (b) 1 else 0
  @inline final def ByteToMinibox(b: Byte): Minibox = b.toLong
  @inline final def CharToMinibox(c: Char): Minibox = c.toLong
  @inline final def ShortToMinibox(s: Short): Minibox = s.toLong
  @inline final def IntToMinibox(i: Int): Minibox = i.toLong
  @inline final def LongToMinibox(l: Long): Minibox = l
  @inline final def DoubleToMinibox(d: Double): Minibox = 
    java.lang.Double.doubleToRawLongBits(d)
  @inline final def FloatToMinibox(f: Float): Minibox = 
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
  @inline final def minibox2box[T](l: Minibox, tag: Tag) : T = {
    val ret: Any = tag match {
      case UNIT => ()
      case BOOLEAN => (l != 0)
      case BYTE => l.toByte 
      case CHAR => l.toChar
      case SHORT => l.toShort
      case INT => l.toInt
      case LONG => l
      case FLOAT => java.lang.Float.intBitsToFloat(l.toInt)
      case DOUBLE => java.lang.Double.longBitsToDouble(l)
    }
    ret.asInstanceOf[T]
  }

//  @inline final def minibox2box[T](l: Long) : T = {
//    null.asInstanceOf[T] // dispatch on tag
//  }

  /*
   *  We do not need to return the type tag also since it is known in the
   *  calling context.
   */
  @inline final def box2minibox(a: Any): Minibox = a match {
    case b : Boolean => if (b) 1 else 0
    case b : Byte => b.toLong
    case u : Unit => 0
    case c : Char => c.toLong
    case s : Short => s.toLong
    case i : Int => i.toLong
    case l : Long => l
    case f : Float => java.lang.Float.floatToIntBits(f).toLong
    case d : Double => java.lang.Double.doubleToRawLongBits(d)
    case _ => sys.error("Trying to unbox a reference type")
  }
}
