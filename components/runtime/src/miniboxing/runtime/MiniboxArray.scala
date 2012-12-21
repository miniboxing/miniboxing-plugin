package miniboxing.runtime
import MiniboxTypes._
import MiniboxConstants._
import MiniboxConversions._

// TODO: Can we use Array[T] and rely on Scala to erase the types?
object MiniboxArray {

  @inline final def newArray[T](len: Int): Array[T] =
    sys.error("I'm just a compile time entity")

  @inline final def internal_newArray(len: Int, tag: Tag): Any = {
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
  }

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
//    array match {
//      case a: Array[Any] => box2minibox(a(idx))
//      case _ =>
        tag match {
          case UNIT =>     array.asInstanceOf[Array[Unit]](idx); 0
          case BOOLEAN =>  if (array.asInstanceOf[Array[Boolean]](idx)) 1 else 0
          case BYTE =>     array.asInstanceOf[Array[Byte]](idx).toLong
          case CHAR =>     array.asInstanceOf[Array[Char]](idx).toLong
          case SHORT =>    array.asInstanceOf[Array[Short]](idx).toLong
          case INT =>      array.asInstanceOf[Array[Int]](idx).toLong
          case LONG =>     array.asInstanceOf[Array[Long]](idx)
          case FLOAT =>    java.lang.Float.floatToIntBits(array.asInstanceOf[Array[Float]](idx)).toLong
          case DOUBLE =>   java.lang.Double.doubleToRawLongBits(array.asInstanceOf[Array[Double]](idx)).toLong
        }
//    }
  }

  @inline final def mbarray_apply_box[T](array: Any, idx: Int, tag: Tag): T = {
//    array match {
//      case a: Array[Any] => a(idx).asInstanceOf[T]
//      case _ =>
        val result = tag match {
          case UNIT =>     array.asInstanceOf[Array[Unit]](idx) = 0
          case BOOLEAN =>  array.asInstanceOf[Array[Boolean]](idx)
          case BYTE =>     array.asInstanceOf[Array[Byte]](idx)
          case CHAR =>     array.asInstanceOf[Array[Char]](idx)
          case SHORT =>    array.asInstanceOf[Array[Short]](idx)
          case INT =>      array.asInstanceOf[Array[Int]](idx)
          case LONG =>     array.asInstanceOf[Array[Long]](idx)
          case FLOAT =>    array.asInstanceOf[Array[Float]](idx)
          case DOUBLE =>   array.asInstanceOf[Array[Double]](idx)
        }
        result.asInstanceOf[T]
//    }
  }

  @inline final def mbarray_update_minibox(array: Any, idx: Int, value: Minibox, tag: Tag): Unit = {
//    array match {
//      case a: Array[Any] => a(idx) = minibox2box(value, tag)
//      case _ =>
        tag match {
          case UNIT =>     array.asInstanceOf[Array[Unit]](idx) = ()
          case BOOLEAN =>  array.asInstanceOf[Array[Boolean]](idx) = if (value == 0) false else true
          case BYTE =>     array.asInstanceOf[Array[Byte]](idx) = value.toByte
          case CHAR =>     array.asInstanceOf[Array[Char]](idx) = value.toChar
          case SHORT =>    array.asInstanceOf[Array[Short]](idx) = value.toShort
          case INT =>      array.asInstanceOf[Array[Int]](idx) = value.toInt
          case LONG =>     array.asInstanceOf[Array[Long]](idx) = value
          case FLOAT =>    array.asInstanceOf[Array[Float]](idx) = java.lang.Float.intBitsToFloat(value.toInt)
          case DOUBLE =>   array.asInstanceOf[Array[Double]](idx) = java.lang.Double.longBitsToDouble(value)
        }
//    }
  }

  @inline final def mbarray_update_box[T](array: Any, idx: Int, value: T, tag: Tag): Unit = {
//    array match {
//      case a: Array[Any] => a(idx) = value
//      case _ =>
        tag match {
          case UNIT =>     array.asInstanceOf[Array[Unit]](idx) = value.asInstanceOf[Unit]
          case BOOLEAN =>  array.asInstanceOf[Array[Boolean]](idx) = value.asInstanceOf[Boolean]
          case BYTE =>     array.asInstanceOf[Array[Byte]](idx) = value.asInstanceOf[Byte]
          case CHAR =>     array.asInstanceOf[Array[Char]](idx) = value.asInstanceOf[Char]
          case SHORT =>    array.asInstanceOf[Array[Short]](idx) = value.asInstanceOf[Short]
          case INT =>      array.asInstanceOf[Array[Int]](idx) = value.asInstanceOf[Int]
          case LONG =>     array.asInstanceOf[Array[Long]](idx) = value.asInstanceOf[Long]
          case FLOAT =>    array.asInstanceOf[Array[Float]](idx) = value.asInstanceOf[Float]
          case DOUBLE =>   array.asInstanceOf[Array[Double]](idx) = value.asInstanceOf[Double]
        }
//    }
  }

  @inline final def mbarray_length(array: Any, tag: Tag): Int =
//    array match {
//      case a: Array[Any] => a.length
//      case _ =>
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
//  }
}
