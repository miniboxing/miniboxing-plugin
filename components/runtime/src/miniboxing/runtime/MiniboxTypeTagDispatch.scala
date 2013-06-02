package miniboxing.runtime

/**
 * We replace
 *   x.toString()
 * by
 *   TypeTagDispatch.toString(x, xTag)
 * in the tree of the method.
 *
 * These definitions will be inlined later during 'inline' phase.
 * So, the size of the code in some class will increase by only a factor
 * proportional to the size of the biggest of these methods (in the worst case)
 */
object MiniboxTypeTagDispatch {
  import MiniboxTypes._
  import MiniboxConversions._

  // import MiniboxConstants._
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
  @inline final def mboxed_toString(x: Minibox, tag: Tag): String =
    tag match {
      // See https://issues.scala-lang.org/browse/SI-6956
      case 0 /* UNIT */ =>    scala.runtime.BoxedUnit.UNIT.toString()
      case 1 /* BOOLEAN */ => scala.runtime.BoxesRunTime.boxToBoolean(x != 0).toString()
      case 5 /* INT */ =>     scala.runtime.BoxesRunTime.boxToInteger(x.toInt).toString()
      case 6 /* LONG */ =>    scala.runtime.BoxesRunTime.boxToLong(x).toString()
      case 2 /* BYTE */ =>    scala.runtime.BoxesRunTime.boxToByte(x.toByte).toString()
      case 3 /* SHORT */ =>   scala.runtime.BoxesRunTime.boxToShort(x.toShort).toString()
      case 4 /* CHAR */ =>    scala.runtime.BoxesRunTime.boxToCharacter(x.toChar).toString()
      case 7 /* FLOAT */ =>
        val ibits = x.toInt
        val fv = java.lang.Float.intBitsToFloat(ibits)
        scala.runtime.BoxesRunTime.boxToFloat(fv).toString()
      case 8 /* DOUBLE */ =>
        val ibits = x.toInt
        val dv = java.lang.Double.longBitsToDouble(x)
        scala.runtime.BoxesRunTime.boxToDouble(dv).toString()
    }

  /*
   * Equality between miniboxed values. Optimized for the case when they have
   * the same type.
   */
  @inline final def mboxed_eqeq(x: Minibox, xtag: Tag, other: Any): Boolean =
    minibox2box(x, xtag) == other

  /*
   * Equality between miniboxed values. Optimized for the case when they have
   * the same type.
   */
  @inline final def mboxed_eqeq(x: Minibox, xtag: Tag, y: Minibox, ytag: Tag): Boolean =
    if (xtag == ytag)
      x == y
    else
      minibox2box(x, xtag) == minibox2box(y, ytag)

  /*
   * Equality between miniboxed values provided that they have the same type
   */
  @inline final def mboxed_eqeq(x: Minibox, y: Minibox): Boolean =
    x == y

  // non-overloaded:
  @inline final def mboxed_eqeq_other(x: Minibox, xtag: Tag, other: Any) =
    mboxed_eqeq(x, xtag, other)

  // non-overloaded:
  @inline final def mboxed_eqeq_tag(x: Minibox, xtag: Tag, y: Minibox, ytag: Tag) =
    mboxed_eqeq(x, xtag, y, ytag)

  // non-overloaded:
  @inline final def mboxed_eqeq_notag(x: Minibox, y: Minibox) =
    mboxed_eqeq(x, y)

  /*
   * Implementation that takes care of the primitive semantics
   */
  @inline final def mboxed_hashCode(x: Minibox, tag: Tag): Int =
    tag match {
      // See https://issues.scala-lang.org/browse/SI-6956
      case 0 /* UNIT */ =>    scala.runtime.BoxedUnit.UNIT.hashCode()
      case 1 /* BOOLEAN */ => scala.runtime.BoxesRunTime.boxToBoolean(x != 0).hashCode()
      case 5 /* INT */ =>     scala.runtime.BoxesRunTime.boxToInteger(x.toInt).hashCode()
      case 6 /* LONG */ =>    scala.runtime.BoxesRunTime.boxToLong(x).hashCode()
      case 2 /* BYTE */ =>    scala.runtime.BoxesRunTime.boxToByte(x.toByte).hashCode()
      case 3 /* SHORT */ =>   scala.runtime.BoxesRunTime.boxToShort(x.toShort).hashCode()
      case 4 /* CHAR */ =>    scala.runtime.BoxesRunTime.boxToCharacter(x.toChar).hashCode()
      case 7 /* FLOAT */ =>
        val ibits = x.toInt
        val fv = java.lang.Float.intBitsToFloat(ibits)
        scala.runtime.BoxesRunTime.boxToFloat(fv).hashCode()
      case 8 /* DOUBLE */ =>
        val ibits = x.toInt
        val dv = java.lang.Double.longBitsToDouble(x)
        scala.runtime.BoxesRunTime.boxToDouble(dv).hashCode()
    }
}
