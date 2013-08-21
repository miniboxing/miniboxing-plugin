package miniboxing.runtime.java;
import miniboxing.runtime.java.MiniboxConstants;

public class MiniboxArray {

  final static Object unitArray = new Object();

  final static <T> Object mbarray_new(int len, byte tag) {
    if (tag > MiniboxConstants.CHAR)
      return mbarray_new_1(len, tag);
    else
      return mbarray_new_2(len, tag);
  }

  final static <T> Object mbarray_new_1(int len, byte tag) {
    switch (tag) {
      case MiniboxConstants.BOOLEAN:
        return new boolean[len];
      case MiniboxConstants.BYTE:
        return new byte[len];
      case MiniboxConstants.SHORT:
        return new short[len];
//      case MiniboxConstants.FLOAT:
//      return new float[len];
//    case MiniboxConstants.DOUBLE:
//      return new double[len];
    }
    return unitArray;
  }

  final static <T> Object mbarray_new_2(int len, byte tag) {
    switch (tag) {
      case MiniboxConstants.CHAR:
        return new char[len];
      case MiniboxConstants.INT:
        return new int[len];
      case MiniboxConstants.LONG:
        return new long[len];
    }
    return unitArray;
  }

//  (tag match {
//    case UNIT =>     new Array[Unit](len)
//    case BOOLEAN =>  new Array[Boolean](len)
//    case BYTE =>     new Array[Byte](len)
//    case CHAR =>     new Array[Char](len)
//    case SHORT =>    new Array[Short](len)
//    case INT =>      new Array[Int](len)
//    case LONG =>     new Array[Long](len)
//    case FLOAT =>    new Array[Float](len)
//    case DOUBLE =>   new Array[Double](len)
//  }).asInstanceOf[Array[T]]
//
//  @inline final def mbarray_apply_minibox(array: Any, idx: Int, tag: Byte): Long = tag match {
//    case INT =>     array.asInstanceOf[Array[Int]](idx).toLong
//    case LONG =>    array.asInstanceOf[Array[Long]](idx)
//    case DOUBLE =>  java.lang.Double.doubleToRawLongBits(array.asInstanceOf[Array[Double]](idx)).toLong
//    case FLOAT =>   java.lang.Float.floatToRawIntBits(array.asInstanceOf[Array[Float]](idx)).toLong
//    case CHAR =>    array.asInstanceOf[Array[Char]](idx).toLong
//    case BYTE =>    array.asInstanceOf[Array[Byte]](idx).toLong
//    case SHORT =>   array.asInstanceOf[Array[Short]](idx).toLong
//    case BOOLEAN => if (array.asInstanceOf[Array[Boolean]](idx)) 1 else 0
//    case UNIT =>    array.asInstanceOf[Array[Unit]](idx); 0
//  }
//
//  @inline final def mbarray_apply_box[T](array: Any, idx: Int, tag: Byte): T = {
//    val result = tag match {
//      case INT =>     array.asInstanceOf[Array[Int]](idx)
//      case LONG =>    array.asInstanceOf[Array[Long]](idx)
//      case DOUBLE =>  array.asInstanceOf[Array[Double]](idx)
//      case FLOAT =>   array.asInstanceOf[Array[Float]](idx)
//      case CHAR =>    array.asInstanceOf[Array[Char]](idx)
//      case BYTE =>    array.asInstanceOf[Array[Byte]](idx)
//      case SHORT =>   array.asInstanceOf[Array[Short]](idx)
//      case BOOLEAN => array.asInstanceOf[Array[Boolean]](idx)
//      case UNIT =>    array.asInstanceOf[Array[Unit]](idx)
//    }
//    result.asInstanceOf[T]
//  }
//
//  @inline final def mbarray_update_minibox(array: Any, idx: Int, value: Long, tag: Byte): Unit = tag match {
//    case INT  =>    array.asInstanceOf[Array[Int]](idx) = value.toInt
//    case LONG =>    array.asInstanceOf[Array[Long]](idx) = value
//    case DOUBLE =>  array.asInstanceOf[Array[Double]](idx) = java.lang.Double.longBitsToDouble(value)
//    case FLOAT =>   array.asInstanceOf[Array[Float]](idx) = java.lang.Float.intBitsToFloat(value.toInt)
//    case CHAR =>    array.asInstanceOf[Array[Char]](idx) = value.toChar
//    case BYTE =>    array.asInstanceOf[Array[Byte]](idx) = value.toByte
//    case SHORT =>   array.asInstanceOf[Array[Short]](idx) = value.toShort
//    case BOOLEAN => array.asInstanceOf[Array[Boolean]](idx) = if (value == 0) false else true
//    case UNIT =>    array.asInstanceOf[Array[Unit]](idx) = ()
//  }
//
//  @inline final def mbarray_update_box[T](array: Any, idx: Int, value: T, tag: Byte): Unit = tag match {
//    case INT =>     array.asInstanceOf[Array[Int]](idx)     = value.asInstanceOf[Int]
//    case LONG =>    array.asInstanceOf[Array[Long]](idx)    = value.asInstanceOf[Long]
//    case DOUBLE =>  array.asInstanceOf[Array[Double]](idx)  = value.asInstanceOf[Double]
//    case FLOAT =>   array.asInstanceOf[Array[Float]](idx)   = value.asInstanceOf[Float]
//    case CHAR =>    array.asInstanceOf[Array[Char]](idx)    = value.asInstanceOf[Char]
//    case BYTE =>    array.asInstanceOf[Array[Byte]](idx)    = value.asInstanceOf[Byte]
//    case SHORT =>   array.asInstanceOf[Array[Short]](idx)   = value.asInstanceOf[Short]
//    case BOOLEAN => array.asInstanceOf[Array[Boolean]](idx) = value.asInstanceOf[Boolean]
//    case UNIT =>    array.asInstanceOf[Array[Unit]](idx)    = value.asInstanceOf[Unit]
//  }
//
//  @inline final def mbarray_length(array: Any, tag: Byte): Int =
//  tag match {
//    case UNIT =>    array.asInstanceOf[Array[Unit]].length
//    case BOOLEAN => array.asInstanceOf[Array[Boolean]].length
//    case BYTE =>    array.asInstanceOf[Array[Byte]].length
//    case CHAR =>    array.asInstanceOf[Array[Char]].length
//    case SHORT =>   array.asInstanceOf[Array[Short]].length
//    case INT =>     array.asInstanceOf[Array[Int]].length
//    case LONG =>    array.asInstanceOf[Array[Long]].length
//    case FLOAT =>   array.asInstanceOf[Array[Float]].length
//    case DOUBLE =>  array.asInstanceOf[Array[Double]].length
//  }
}

