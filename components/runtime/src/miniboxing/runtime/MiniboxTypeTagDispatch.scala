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
  @inline final def mboxed_toString(x: Minibox, tag: Tag): String = tag match {
    // See https://issues.scala-lang.org/browse/SI-6956
    case 0 /* UNIT */ => "()"
    case 1 /* BOOLEAN */ => if (x != 0) "true" else "false"
    case 2 /* BYTE */ | 3 /* SHORT */ | 5 /* INT */ | 6 /* LONG */  => java.lang.Long.toString(x)
    case 4 /* CHAR */ => java.lang.Character.toString(x.toChar)
    case 7 /* FLOAT */ =>
      java.lang.Float.toString(java.lang.Float.intBitsToFloat(x.toInt))
    case 8 /* DOUBLE */ =>
      java.lang.Double.toString(java.lang.Double.longBitsToDouble(x))
  }

  /*
   * Equality between miniboxed values. Optimized for the case when they have
   * the same type.
   */
  @inline final def mboxed_eqeq(x: Minibox, xtag: Tag, y: Minibox, ytag: Tag): Boolean = {
    if (xtag == ytag) {
      x == y
    } else {
      minibox2box(x, xtag) == minibox2box(y, ytag)
    }
  }
  /*
   * Equality between miniboxed values provided that they have the same type
   */
  @inline final def mboxed_eqeq(x: Minibox, y: Minibox): Boolean = {
    x == y
  }

  /*
   * Implementation that takes care of the primitive semantics
   */
  @inline final def mboxed_hashhash(x: Minibox, tag: Tag): Int = tag match {
    // See https://issues.scala-lang.org/browse/SI-6956
    case 0 /* UNIT */ | 1 /* BOOLEAN */ | 5 /* INT */ | 6 /* LONG */ => x.toInt
    case 2 /* BYTE */ => x.toByte.toInt
    case 3 /* SHORT */ => x.toShort.toInt
    case 4 /* CHAR */ => x.toChar.toInt
    case 7 /* FLOAT */ =>
      val ibits = x.toInt
      val fv = java.lang.Float.intBitsToFloat(ibits)
      val iv = fv.toInt
      if (iv.toFloat == fv) iv else ibits
    case 8 /* DOUBLE */ =>
      val ibits = x.toInt
      val dv = java.lang.Double.longBitsToDouble(x)
      val iv = dv.toInt
      if (iv.toDouble == dv) iv else ibits
  }

  @inline final def mboxed_hashCode(x: Minibox, tag: Tag): Int = mboxed_hashhash(x, tag)
}
