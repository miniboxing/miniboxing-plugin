package runtime

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
  import MiniboxConstants._

  /*
   *  For methods like `toString` or `equals` there is not much improvement
   *  to be expected. Just use boxing for them. 
   */
  @inline final def toString(x: Minibox, tag: Tag): String = tag match {
    case UNIT => "()"
    case BOOLEAN => if (x != 0) "true" else "false"
    case INT | LONG | SHORT | BYTE => java.lang.Long.toString(x)
    case CHAR => java.lang.Character.toString(x.toChar)
    case FLOAT =>
      java.lang.Float.toString(java.lang.Float.intBitsToFloat(x.toInt))
    case DOUBLE =>
      java.lang.Double.toString(java.lang.Double.longBitsToDouble(x))
  }

  /*
   * Equality between miniboxed values. Optimized for the case when they have
   * the same type.
   */
  @inline final def eqeq(x: Minibox, xtag: Tag, y: Minibox, ytag: Tag): Boolean = {
    if (xtag == ytag) {
      x == y
    } else {
      minibox2box(x, xtag) == minibox2box(y, ytag)
    }
  }
  /*
   * Equality between miniboxed values provided that they have the same type
   */
  @inline final def eqeq(x: Minibox, y: Minibox): Boolean = {
    x == y
  }

  /*
   * Implementation that takes care of the primitive semantics
   */
  @inline final def hashhash(x: Minibox, tag: Tag): Int = tag match {
    case UNIT | BOOLEAN | INT | LONG => x.toInt
    case BYTE => x.toByte.toInt
    case CHAR => x.toChar.toInt
    case SHORT => x.toShort.toInt
    case FLOAT =>
      val ibits = x.toInt
      val fv = java.lang.Float.intBitsToFloat(ibits)
      val iv = fv.toInt
      if (iv.toFloat == fv) iv else ibits
    case DOUBLE =>
      val ibits = x.toInt
      val dv = java.lang.Double.longBitsToDouble(x)
      val iv = dv.toInt
      if (iv.toDouble == dv) iv else ibits
  }

  @inline final def hashCode(x: Minibox, tag: Tag): Int = x.hashCode

  @inline final def array_apply(array: Any, pos: Int, tag: Tag): Long = {
    val elem: Minibox = tag match {
      case UNIT =>
        0
      case BOOLEAN =>
        BooleanToMinibox(array.asInstanceOf[Array[Boolean]](pos))
      case BYTE =>
        ByteToMinibox(array.asInstanceOf[Array[Byte]](pos))
      case CHAR =>
        CharToMinibox(array.asInstanceOf[Array[Char]](pos))
      case SHORT =>
        ShortToMinibox(array.asInstanceOf[Array[Short]](pos))
      case INT =>
        IntToMinibox(array.asInstanceOf[Array[Int]](pos))
      case LONG =>
        array.asInstanceOf[Array[Long]](pos)
      case FLOAT =>
        FloatToMinibox(array.asInstanceOf[Array[Float]](pos))
      case DOUBLE =>
        DoubleToMinibox(array.asInstanceOf[Array[Double]](pos))
    }
    elem
  }

  @inline final def array_update[T](array: Any, pos: Int, x: Minibox, tag: Tag): Unit = tag match {
    case UNIT =>
      array.asInstanceOf[Array[Unit]](pos) = MiniboxToUnit(x)
    case BOOLEAN =>
      array.asInstanceOf[Array[Boolean]](pos) = MiniboxToBoolean(x)
    case BYTE =>
      array.asInstanceOf[Array[Byte]](pos) = MiniboxToByte(x)
    case CHAR =>
      array.asInstanceOf[Array[Char]](pos) = MiniboxToChar(x)
    case SHORT =>
      array.asInstanceOf[Array[Short]](pos) = MiniboxToShort(x)
    case INT =>
      array.asInstanceOf[Array[Int]](pos) = MiniboxToInt(x)
    case LONG =>
      array.asInstanceOf[Array[Long]](pos) = MiniboxToLong(x)
    case FLOAT =>
      array.asInstanceOf[Array[Float]](pos) = MiniboxToFloat(x)
    case DOUBLE =>
      array.asInstanceOf[Array[Double]](pos) = MiniboxToDouble(x)

  }

  @inline final def array_length(array: Any, tag: Tag): Int = tag match {
    case UNIT =>
      array.asInstanceOf[Array[Unit]].length
    case BOOLEAN =>
      array.asInstanceOf[Array[Boolean]].length
    case BYTE =>
      array.asInstanceOf[Array[Byte]].length
    case CHAR =>
      array.asInstanceOf[Array[Char]].length
    case SHORT =>
      array.asInstanceOf[Array[Short]].length
    case INT =>
      array.asInstanceOf[Array[Int]].length
    case LONG =>
      array.asInstanceOf[Array[Long]].length
    case FLOAT =>
      array.asInstanceOf[Array[Float]].length
    case DOUBLE =>
      array.asInstanceOf[Array[Double]].length
  }
}