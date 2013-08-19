package miniboxing.runtime

/**
 * We replace
 *   x.toString()
 * by
 *   TypeByteDispatch.toString(x, xByte)
 * in the tree of the method.
 *
 * These definitions will be inlined later during 'inline' phase.
 * So, the size of the code in some class will increase by only a factor
 * proportional to the size of the biggest of these methods (in the worst case)
 */
object MiniboxDispatch {

  import MiniboxConversions._

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
   *  For methods like `toString` or `equals` there is not much improvement
   *  to be expected. Just use boxing for them.
   */
  @inline final def mboxed_toString(x: Long, tag: Byte): String =
    tag match {
      // See https://issues.scala-lang.org/browse/SI-6956
      case UNIT =>    scala.runtime.BoxedUnit.UNIT.toString()
      case BOOLEAN => scala.runtime.BoxesRunTime.boxToBoolean(x != 0).toString()
      case INT =>     scala.runtime.BoxesRunTime.boxToInteger(x.toInt).toString()
      case LONG =>    scala.runtime.BoxesRunTime.boxToLong(x).toString()
      case BYTE =>    scala.runtime.BoxesRunTime.boxToByte(x.toByte).toString()
      case SHORT =>   scala.runtime.BoxesRunTime.boxToShort(x.toShort).toString()
      case CHAR =>    scala.runtime.BoxesRunTime.boxToCharacter(x.toChar).toString()
      case FLOAT =>
        val ibits = x.toInt
        val fv = java.lang.Float.intBitsToFloat(ibits)
        scala.runtime.BoxesRunTime.boxToFloat(fv).toString()
      case DOUBLE =>
        val ibits = x.toInt
        val dv = java.lang.Double.longBitsToDouble(x)
        scala.runtime.BoxesRunTime.boxToDouble(dv).toString()
    }

  /*
   * Equality between miniboxed values. Optimized for the case when they have
   * the same type.
   */
  @inline final def mboxed_eqeq(x: Long, xtag: Byte, other: Any): Boolean =
    minibox2box(x, xtag) == other

  /*
   * Equality between miniboxed values. Optimized for the case when they have
   * the same type.
   */
  @inline final def mboxed_eqeq(x: Long, xtag: Byte, y: Long, ytag: Byte): Boolean =
    if (xtag == ytag)
      x == y
    else
      minibox2box(x, xtag) == minibox2box(y, ytag)

  /*
   * Equality between miniboxed values provided that they have the same type
   */
  @inline final def mboxed_eqeq(x: Long, y: Long): Boolean =
    x == y

  // non-overloaded:
  @inline final def mboxed_eqeq_other(x: Long, xtag: Byte, other: Any) =
    mboxed_eqeq(x, xtag, other)

  // non-overloaded:
  @inline final def mboxed_eqeq_tag(x: Long, xtag: Byte, y: Long, ytag: Byte) =
    mboxed_eqeq(x, xtag, y, ytag)

  // non-overloaded:
  @inline final def mboxed_eqeq_notag(x: Long, y: Long) =
    mboxed_eqeq(x, y)

  /*
   * Implementation that takes care of the primitive semantics
   */
  @inline final def mboxed_hashCode(x: Long, tag: Byte): Int =
    tag match {
      // See https://issues.scala-lang.org/browse/SI-6956
      case UNIT =>    scala.runtime.BoxedUnit.UNIT.hashCode()
      case BOOLEAN => scala.runtime.BoxesRunTime.boxToBoolean(x != 0).hashCode()
      case INT =>     scala.runtime.BoxesRunTime.boxToInteger(x.toInt).hashCode()
      case LONG =>    scala.runtime.BoxesRunTime.boxToLong(x).hashCode()
      case BYTE =>    scala.runtime.BoxesRunTime.boxToByte(x.toByte).hashCode()
      case SHORT =>   scala.runtime.BoxesRunTime.boxToShort(x.toShort).hashCode()
      case CHAR =>    scala.runtime.BoxesRunTime.boxToCharacter(x.toChar).hashCode()
      case FLOAT =>
        val ibits = x.toInt
        val fv = java.lang.Float.intBitsToFloat(ibits)
        scala.runtime.BoxesRunTime.boxToFloat(fv).hashCode()
      case DOUBLE =>
        val ibits = x.toInt
        val dv = java.lang.Double.longBitsToDouble(x)
        scala.runtime.BoxesRunTime.boxToDouble(dv).hashCode()
    }
}
