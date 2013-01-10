package miniboxing.runtime
import MiniboxTypes._
//import MiniboxConstants._
import MiniboxConversions._

// TODO: Can we use Array[T] and rely on Scala to erase the types?
object MiniboxArray {

  // import MiniboxConstants._
  // instead of importing, we copy the values here so the Scala compiler transforms the calls into tableswitches
//  private[this] final val UNIT = 0;
//  private[this] final val BOOLEAN = 1;
//  private[this] final val BYTE = 2;
//  private[this] final val SHORT = 3;
//  private[this] final val CHAR = 4;
//  private[this] final val INT = 5;
//  private[this] final val LONG = 6;
//  private[this] final val FLOAT = 7;
//  private[this] final val DOUBLE = 8;
//  private[this] final val REFERENCE = 9;

  @inline final def newArray[T](len: Int): Array[T] =
    sys.error("I'm just a compile time entity")

  @inline final def internal_newArray(len: Int, tag: Tag): Any = {
    tag.asInstanceOf[Byte] match {
      // See https://issues.scala-lang.org/browse/SI-6956
      case 0 /* UNIT */ =>     new Array[Unit](len)
      case 1 /* BOOLEAN */ =>  new Array[Boolean](len)
      case 2 /* BYTE */ =>     new Array[Byte](len)
      case 3 /* CHAR */ =>     new Array[Char](len)
      case 4 /* SHORT */ =>    new Array[Short](len)
      case 5 /* INT */ =>      new Array[Int](len)
      case 6 /* LONG */ =>     new Array[Long](len)
      case 7 /* FLOAT */ =>    new Array[Float](len)
      case 8 /* DOUBLE */ =>   new Array[Double](len)
    }
  }

  @inline final def mbarray_new(len: Int, tag: Tag): Any =
    tag.asInstanceOf[Byte] match {
      // See https://issues.scala-lang.org/browse/SI-6956
      case 0 /* UNIT */ =>     new Array[Unit](len)
      case 1 /* BOOLEAN */ =>  new Array[Boolean](len)
      case 2 /* BYTE */ =>     new Array[Byte](len)
      case 3 /* CHAR */ =>     new Array[Char](len)
      case 4 /* SHORT */ =>    new Array[Short](len)
      case 5 /* INT */ =>      new Array[Int](len)
      case 6 /* LONG */ =>     new Array[Long](len)
      case 7 /* FLOAT */ =>    new Array[Float](len)
      case 8 /* DOUBLE */ =>   new Array[Double](len)
    }

  @inline final def mbarray_apply_minibox(array: Any, idx: Int, tag: Tag): Minibox = {
//    array match {
//      case a: Array[Any] => box2minibox(a(idx))
//      case _ =>
        tag.asInstanceOf[Byte] match {
          // See https://issues.scala-lang.org/browse/SI-6956
          case 0 /* UNIT */ =>     array.asInstanceOf[Array[Unit]](idx); 0
          case 1 /* BOOLEAN */ =>  if (array.asInstanceOf[Array[Boolean]](idx)) 1 else 0
          case 2 /* BYTE */ =>     array.asInstanceOf[Array[Byte]](idx).toLong
          case 3 /* CHAR */ =>     array.asInstanceOf[Array[Char]](idx).toLong
          case 4 /* SHORT */ =>    array.asInstanceOf[Array[Short]](idx).toLong
          case 5 /* INT */ =>      array.asInstanceOf[Array[Int]](idx).toLong
          case 6 /* LONG */ =>     array.asInstanceOf[Array[Long]](idx)
          case 7 /* FLOAT */ =>    java.lang.Float.floatToIntBits(array.asInstanceOf[Array[Float]](idx)).toLong
          case 8 /* DOUBLE */ =>   java.lang.Double.doubleToRawLongBits(array.asInstanceOf[Array[Double]](idx)).toLong
        }
//    }
  }

  @inline final def mbarray_apply_box[T](array: Any, idx: Int, tag: Tag): T = {
//    array match {
//      case a: Array[Any] => a(idx).asInstanceOf[T]
//      case _ =>
        val result = tag.asInstanceOf[Byte] match {
          // See https://issues.scala-lang.org/browse/SI-6956
          case 0 /* UNIT */ =>     array.asInstanceOf[Array[Unit]](idx) = 0
          case 1 /* BOOLEAN */ =>  array.asInstanceOf[Array[Boolean]](idx)
          case 2 /* BYTE */ =>     array.asInstanceOf[Array[Byte]](idx)
          case 3 /* CHAR */ =>     array.asInstanceOf[Array[Char]](idx)
          case 4 /* SHORT */ =>    array.asInstanceOf[Array[Short]](idx)
          case 5 /* INT */ =>      array.asInstanceOf[Array[Int]](idx)
          case 6 /* LONG */ =>     array.asInstanceOf[Array[Long]](idx)
          case 7 /* FLOAT */ =>    array.asInstanceOf[Array[Float]](idx)
          case 8 /* DOUBLE */ =>   array.asInstanceOf[Array[Double]](idx)
        }
        result.asInstanceOf[T]
//    }
  }

  @inline final def mbarray_update_minibox(array: Any, idx: Int, value: Minibox, tag: Tag): Unit = {
//    array match {
//      case a: Array[Any] => a(idx) = minibox2box(value, tag)
//      case _ =>
        tag.asInstanceOf[Byte] match {
          // See https://issues.scala-lang.org/browse/SI-6956
          case 0 /* UNIT */ =>     array.asInstanceOf[Array[Unit]](idx) = ()
          case 1 /* BOOLEAN */ =>  array.asInstanceOf[Array[Boolean]](idx) = if (value == 0) false else true
          case 2 /* BYTE */ =>     array.asInstanceOf[Array[Byte]](idx) = value.toByte
          case 3 /* CHAR */ =>     array.asInstanceOf[Array[Char]](idx) = value.toChar
          case 4 /* SHORT */ =>    array.asInstanceOf[Array[Short]](idx) = value.toShort
          case 5 /* INT */ =>      array.asInstanceOf[Array[Int]](idx) = value.toInt
          case 6 /* LONG */ =>     array.asInstanceOf[Array[Long]](idx) = value
          case 7 /* FLOAT */ =>    array.asInstanceOf[Array[Float]](idx) = java.lang.Float.intBitsToFloat(value.toInt)
          case 8 /* DOUBLE */ =>   array.asInstanceOf[Array[Double]](idx) = java.lang.Double.longBitsToDouble(value)
        }
//    }
  }

  @inline final def mbarray_update_box[T](array: Any, idx: Int, value: T, tag: Tag): Unit = {
//    array match {
//      case a: Array[Any] => a(idx) = value
//      case _ =>
        tag.asInstanceOf[Byte] match {
          // See https://issues.scala-lang.org/browse/SI-6956
          case 0 /* UNIT */ =>     array.asInstanceOf[Array[Unit]](idx) = value.asInstanceOf[Unit]
          case 1 /* BOOLEAN */ =>  array.asInstanceOf[Array[Boolean]](idx) = value.asInstanceOf[Boolean]
          case 2 /* BYTE */ =>     array.asInstanceOf[Array[Byte]](idx) = value.asInstanceOf[Byte]
          case 3 /* CHAR */ =>     array.asInstanceOf[Array[Char]](idx) = value.asInstanceOf[Char]
          case 4 /* SHORT */ =>    array.asInstanceOf[Array[Short]](idx) = value.asInstanceOf[Short]
          case 5 /* INT */ =>      array.asInstanceOf[Array[Int]](idx) = value.asInstanceOf[Int]
          case 6 /* LONG */ =>     array.asInstanceOf[Array[Long]](idx) = value.asInstanceOf[Long]
          case 7 /* FLOAT */ =>    array.asInstanceOf[Array[Float]](idx) = value.asInstanceOf[Float]
          case 8 /* DOUBLE */ =>   array.asInstanceOf[Array[Double]](idx) = value.asInstanceOf[Double]
        }
//    }
  }

  @inline final def mbarray_length(array: Any, tag: Tag): Int =
//    array match {
//      case a: Array[Any] => a.length
//      case _ =>
        tag.asInstanceOf[Byte] match {
          // See https://issues.scala-lang.org/browse/SI-6956
          case 0 /* UNIT */ =>    array.asInstanceOf[Array[Unit]].length
          case 1 /* BOOLEAN */ => array.asInstanceOf[Array[Boolean]].length
          case 2 /* BYTE */ =>    array.asInstanceOf[Array[Byte]].length
          case 3 /* CHAR */ =>    array.asInstanceOf[Array[Char]].length
          case 4 /* SHORT */ =>   array.asInstanceOf[Array[Short]].length
          case 5 /* INT */ =>     array.asInstanceOf[Array[Int]].length
          case 6 /* LONG */ =>    array.asInstanceOf[Array[Long]].length
          case 7 /* FLOAT */ =>   array.asInstanceOf[Array[Float]].length
          case 8 /* DOUBLE */ =>  array.asInstanceOf[Array[Double]].length
        }
//  }
}
