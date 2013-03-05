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

  @inline final def mbarray_apply_minibox(array: Any, idx: Int, tag: Tag): Minibox = tag match {
    case INT =>     array.asInstanceOf[Array[Int]](idx).toLong
    case LONG =>    array.asInstanceOf[Array[Long]](idx)
    case DOUBLE =>  java.lang.Double.doubleToRawLongBits(array.asInstanceOf[Array[Double]](idx)).toLong
    case FLOAT =>   java.lang.Float.floatToRawIntBits(array.asInstanceOf[Array[Float]](idx)).toLong
    case CHAR =>    array.asInstanceOf[Array[Char]](idx).toLong
    case BYTE =>    array.asInstanceOf[Array[Byte]](idx).toLong
    case SHORT =>   array.asInstanceOf[Array[Short]](idx).toLong
    case BOOLEAN => if (array.asInstanceOf[Array[Boolean]](idx)) 1 else 0
    case UNIT =>    array.asInstanceOf[Array[Unit]](idx); 0
  }

  @inline final def mbarray_apply_box[T](array: Any, idx: Int, tag: Tag): T = {
    val result = tag match {
      case INT =>     array.asInstanceOf[Array[Int]](idx)
      case LONG =>    array.asInstanceOf[Array[Long]](idx)
      case DOUBLE =>  array.asInstanceOf[Array[Double]](idx)
      case FLOAT =>   array.asInstanceOf[Array[Float]](idx)
      case CHAR =>    array.asInstanceOf[Array[Char]](idx)
      case BYTE =>    array.asInstanceOf[Array[Byte]](idx)
      case SHORT =>   array.asInstanceOf[Array[Short]](idx)
      case BOOLEAN => array.asInstanceOf[Array[Boolean]](idx)
      case UNIT =>    array.asInstanceOf[Array[Unit]](idx)
    }
    result.asInstanceOf[T]
  }

  @inline final def mbarray_update_minibox(array: Any, idx: Int, value: Minibox, tag: Tag): Unit = tag match {
    case INT  =>    array.asInstanceOf[Array[Int]](idx) = value.toInt
    case LONG =>    array.asInstanceOf[Array[Long]](idx) = value
    case DOUBLE =>  array.asInstanceOf[Array[Double]](idx) = java.lang.Double.longBitsToDouble(value)
    case FLOAT =>   array.asInstanceOf[Array[Float]](idx) = java.lang.Float.intBitsToFloat(value.toInt)
    case CHAR =>    array.asInstanceOf[Array[Char]](idx) = value.toChar
    case BYTE =>    array.asInstanceOf[Array[Byte]](idx) = value.toByte
    case SHORT =>   array.asInstanceOf[Array[Short]](idx) = value.toShort
    case BOOLEAN => array.asInstanceOf[Array[Boolean]](idx) = if (value == 0) false else true
    case UNIT =>    array.asInstanceOf[Array[Unit]](idx) = ()
  }

  @inline final def mbarray_update_box[T](array: Any, idx: Int, value: T, tag: Tag): Unit = tag match {
      case INT =>     array.asInstanceOf[Array[Int]](idx)     = value.asInstanceOf[Int]
      case LONG =>    array.asInstanceOf[Array[Long]](idx)    = value.asInstanceOf[Long]
      case DOUBLE =>  array.asInstanceOf[Array[Double]](idx)  = value.asInstanceOf[Double]
      case FLOAT =>   array.asInstanceOf[Array[Float]](idx)   = value.asInstanceOf[Float]
      case CHAR =>    array.asInstanceOf[Array[Char]](idx)    = value.asInstanceOf[Char]
      case BYTE =>    array.asInstanceOf[Array[Byte]](idx)    = value.asInstanceOf[Byte]
      case SHORT =>   array.asInstanceOf[Array[Short]](idx)   = value.asInstanceOf[Short]
      case BOOLEAN => array.asInstanceOf[Array[Boolean]](idx) = value.asInstanceOf[Boolean]
      case UNIT =>    array.asInstanceOf[Array[Unit]](idx)    = value.asInstanceOf[Unit]
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
