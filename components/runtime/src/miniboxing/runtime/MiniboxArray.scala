package miniboxing.runtime
import MiniboxTypes._
//import MiniboxConstants._
import MiniboxConversions._

object MiniboxArray {

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

  @inline final def mbarray_new(len: Int, tag: Tag): Any =
    tag match {
      case UNIT =>     new Array[Unit](len)
      case BOOLEAN =>  new Array[Boolean](len)
      case BYTE =>     new Array[Byte](len)
      case CHAR =>     new Array[Char](len)
      case SHORT =>    new Array[Short](len)
      case INT =>      new Array[Int](len)
      case LONG =>     new Array[Long](len)
      case FLOAT =>    new Array[Float](len)
      case DOUBLE =>   new Array[Double](len)
    }

  @inline final def mbarray_apply_minibox(array: Any, idx: Int, tag: Tag): Minibox = {
    if (tag == INT)          { array.asInstanceOf[Array[Int]](idx).toLong }
    else if (tag == LONG)    { array.asInstanceOf[Array[Long]](idx) }
    else if (tag == DOUBLE)  { java.lang.Double.doubleToRawLongBits(array.asInstanceOf[Array[Double]](idx)).toLong }
    else if (tag == FLOAT)   { java.lang.Float.floatToRawIntBits(array.asInstanceOf[Array[Float]](idx)).toLong }
    else if (tag == CHAR)    { array.asInstanceOf[Array[Char]](idx).toLong }
    else if (tag == BYTE)    { array.asInstanceOf[Array[Byte]](idx).toLong }
    else if (tag == SHORT)   { array.asInstanceOf[Array[Short]](idx).toLong }
    else if (tag == BOOLEAN) { if (array.asInstanceOf[Array[Boolean]](idx)) 1 else 0 }
    else if (tag == UNIT)    { array.asInstanceOf[Array[Unit]](idx); 0 }
    else ???
  }

  @inline final def mbarray_apply_box[T](array: Any, idx: Int, tag: Tag): T = {
    val result =
      if (tag == INT)          { array.asInstanceOf[Array[Int]](idx) }
      else if (tag == LONG)    { array.asInstanceOf[Array[Long]](idx) }
      else if (tag == DOUBLE)  { array.asInstanceOf[Array[Double]](idx) }
      else if (tag == FLOAT)   { array.asInstanceOf[Array[Float]](idx) }
      else if (tag == CHAR)    { array.asInstanceOf[Array[Char]](idx) }
      else if (tag == BYTE)    { array.asInstanceOf[Array[Byte]](idx) }
      else if (tag == SHORT)   { array.asInstanceOf[Array[Short]](idx) }
      else if (tag == BOOLEAN) { array.asInstanceOf[Array[Boolean]](idx) }
      else if (tag == UNIT)    { array.asInstanceOf[Array[Unit]](idx) }
      else ???
    result.asInstanceOf[T]
  }

  @inline final def mbarray_update_minibox(array: Any, idx: Int, value: Minibox, tag: Tag): Unit = {
    if (tag == INT)          { array.asInstanceOf[Array[Int]](idx) = value.toInt }
    else if (tag == LONG)    { array.asInstanceOf[Array[Long]](idx) = value }
    else if (tag == DOUBLE)  { array.asInstanceOf[Array[Double]](idx) = java.lang.Double.longBitsToDouble(value) }
    else if (tag == FLOAT)   { array.asInstanceOf[Array[Float]](idx) = java.lang.Float.intBitsToFloat(value.toInt) }
    else if (tag == CHAR)    { array.asInstanceOf[Array[Char]](idx) = value.toChar }
    else if (tag == BYTE)    { array.asInstanceOf[Array[Byte]](idx) = value.toByte }
    else if (tag == SHORT)   { array.asInstanceOf[Array[Short]](idx) = value.toShort }
    else if (tag == BOOLEAN) { array.asInstanceOf[Array[Boolean]](idx) = if (value == 0) false else true }
    else if (tag == UNIT)    { array.asInstanceOf[Array[Unit]](idx) = () }
    else ???
  }

  @inline final def mbarray_update_box[T](array: Any, idx: Int, value: T, tag: Tag): Unit = {
    val result =
      if (tag == INT)          { array.asInstanceOf[Array[Int]](idx)     = value.asInstanceOf[Int]}
      else if (tag == LONG)    { array.asInstanceOf[Array[Long]](idx)    = value.asInstanceOf[Long] }
      else if (tag == DOUBLE)  { array.asInstanceOf[Array[Double]](idx)  = value.asInstanceOf[Double] }
      else if (tag == FLOAT)   { array.asInstanceOf[Array[Float]](idx)   = value.asInstanceOf[Float] }
      else if (tag == CHAR)    { array.asInstanceOf[Array[Char]](idx)    = value.asInstanceOf[Char] }
      else if (tag == BYTE)    { array.asInstanceOf[Array[Byte]](idx)    = value.asInstanceOf[Byte] }
      else if (tag == SHORT)   { array.asInstanceOf[Array[Short]](idx)   = value.asInstanceOf[Short] }
      else if (tag == BOOLEAN) { array.asInstanceOf[Array[Boolean]](idx) = value.asInstanceOf[Boolean] }
      else if (tag == UNIT)    { array.asInstanceOf[Array[Unit]](idx)    = value.asInstanceOf[Unit] }
      else ???
  }

  @inline final def mbarray_length(array: Any, tag: Tag): Int =
    tag match {
      case UNIT =>    array.asInstanceOf[Array[Unit]].length
      case BOOLEAN => array.asInstanceOf[Array[Boolean]].length
      case BYTE =>    array.asInstanceOf[Array[Byte]].length
      case CHAR =>    array.asInstanceOf[Array[Char]].length
      case SHORT =>   array.asInstanceOf[Array[Short]].length
      case INT =>     array.asInstanceOf[Array[Int]].length
      case LONG =>    array.asInstanceOf[Array[Long]].length
      case FLOAT =>   array.asInstanceOf[Array[Float]].length
      case DOUBLE =>  array.asInstanceOf[Array[Double]].length
    }
}
