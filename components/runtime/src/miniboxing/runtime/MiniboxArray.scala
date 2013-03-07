package miniboxing.runtime
import MiniboxTypes._
import MiniboxConversions._


/**********************************************************************************************************************\
\**********************************************************************************************************************/

class MiniboxArray_NoInline {

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

  def mbarray_new(len: Int, tag: Tag): Any = tag match {
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

   def mbarray_apply_minibox(array: Any, idx: Int, tag: Tag): Minibox = tag match {
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

   def mbarray_apply_box[T](array: Any, idx: Int, tag: Tag): T = {
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

   def mbarray_update_minibox(array: Any, idx: Int, value: Minibox, tag: Tag): Unit = tag match {
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

  def mbarray_update_box[T](array: Any, idx: Int, value: T, tag: Tag): Unit = tag match {
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

  def mbarray_length(array: Any, tag: Tag): Int =
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
object `package` {
  val MiniboxArray_NoInline = new MiniboxArray_NoInline()
}

/**********************************************************************************************************************\
\**********************************************************************************************************************/

object MiniboxArray_FullSwitch {

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

/**********************************************************************************************************************\
\**********************************************************************************************************************/

object MiniboxArray_SemiSwitch {

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
    if (tag % 2 == 0) {
      tag match {
        case UNIT =>     new Array[Unit](len)
        case BYTE =>     new Array[Byte](len)
        case CHAR =>     new Array[Char](len)
        case LONG =>     new Array[Long](len)
        case DOUBLE =>   new Array[Double](len)
      }
    } else {
      tag match {
        case BOOLEAN =>  new Array[Boolean](len)
        case SHORT =>    new Array[Short](len)
        case INT =>      new Array[Int](len)
        case FLOAT =>    new Array[Float](len)
      }
    }

  @inline final def mbarray_apply_minibox(array: Any, idx: Int, tag: Tag): Minibox =
    if (tag % 2 == 0) {
      tag match {
        case UNIT =>    array.asInstanceOf[Array[Unit]](idx); 0
        case BYTE =>    array.asInstanceOf[Array[Byte]](idx).toLong
        case CHAR =>    array.asInstanceOf[Array[Char]](idx).toLong
        case LONG =>    array.asInstanceOf[Array[Long]](idx)
        case DOUBLE =>  java.lang.Double.doubleToRawLongBits(array.asInstanceOf[Array[Double]](idx)).toLong
      }
    } else {
      tag match {
        case INT =>     array.asInstanceOf[Array[Int]](idx).toLong
        case FLOAT =>   java.lang.Float.floatToRawIntBits(array.asInstanceOf[Array[Float]](idx)).toLong
        case SHORT =>   array.asInstanceOf[Array[Short]](idx).toLong
        case BOOLEAN => if (array.asInstanceOf[Array[Boolean]](idx)) 1 else 0
      }
    }

  @inline final def mbarray_apply_box[T](array: Any, idx: Int, tag: Tag): T = ???

  @inline final def mbarray_update_minibox(array: Any, idx: Int, value: Minibox, tag: Tag): Unit =
    if (tag % 2 == 0) {
      tag match {
        case UNIT =>    array.asInstanceOf[Array[Unit]](idx) = ()
        case BYTE =>    array.asInstanceOf[Array[Byte]](idx) = value.toByte
        case CHAR =>    array.asInstanceOf[Array[Char]](idx) = value.toChar
        case LONG =>    array.asInstanceOf[Array[Long]](idx) = value
        case DOUBLE =>  array.asInstanceOf[Array[Double]](idx) = java.lang.Double.longBitsToDouble(value)
      }
    } else {
      tag match {
        case FLOAT =>   array.asInstanceOf[Array[Float]](idx) = java.lang.Float.intBitsToFloat(value.toInt)
        case INT  =>    array.asInstanceOf[Array[Int]](idx) = value.toInt
        case SHORT =>   array.asInstanceOf[Array[Short]](idx) = value.toShort
        case BOOLEAN => array.asInstanceOf[Array[Boolean]](idx) = if (value == 0) false else true
      }
    }

  @inline final def mbarray_update_box[T](array: Any, idx: Int, value: T, tag: Tag): Unit = ???

  @inline final def mbarray_length(array: Any, tag: Tag): Int =
    if (tag % 2 == 0) {
      tag match {
        case UNIT =>    array.asInstanceOf[Array[Unit]].length
        case BYTE =>    array.asInstanceOf[Array[Byte]].length
        case CHAR =>    array.asInstanceOf[Array[Char]].length
        case LONG =>    array.asInstanceOf[Array[Long]].length
        case DOUBLE =>  array.asInstanceOf[Array[Double]].length
      }
    } else {
      tag match {
        case BOOLEAN => array.asInstanceOf[Array[Boolean]].length
        case SHORT =>   array.asInstanceOf[Array[Short]].length
        case INT =>     array.asInstanceOf[Array[Int]].length
        case FLOAT =>   array.asInstanceOf[Array[Float]].length
      }
    }
}

/**********************************************************************************************************************\
\**********************************************************************************************************************/

object MiniboxArray_DecisionTrees {

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

  @inline final def mbarray_new(len: Int, tag: Tag): Any = {
    if (tag % 2 == 0) {
      if (tag % 4 == 0) {
        if (tag == DOUBLE) {
          new Array[Double](len)
        } else if (tag == SHORT) {
          new Array[Short](len)
        } else // UNIT
        new Array[Unit](len)
    } else {
      if (tag == LONG) {
        new Array[Long](len)
      } else // BYTE
        new Array[Byte](len)
    }
  } else {
    if (tag % 4 == 1) {
      if (tag == INT) {
        new Array[Int](len)
      } else { // BOOLEAN
        new Array[Boolean](len)
      }
    } else {
      if (tag == FLOAT) {
        new Array[Float](len)
      } else { // CHAR
        new Array[Char](len)
      }
    }
  }
}

@inline final def mbarray_apply_minibox(array: Any, idx: Int, tag: Tag): Minibox = {
  if (tag % 2 == 0) {
    if (tag % 4 == 0) {
      if (tag == DOUBLE) {
        java.lang.Double.doubleToRawLongBits(array.asInstanceOf[Array[Double]](idx)).toLong
      } else if (tag == SHORT) {
        array.asInstanceOf[Array[Short]](idx).toLong
      } else // UNIT
        array.asInstanceOf[Array[Unit]](idx); 0
    } else {
      if (tag == LONG) {
        array.asInstanceOf[Array[Long]](idx)
      } else // BYTE
        array.asInstanceOf[Array[Byte]](idx).toLong
    }
  } else {
    if (tag % 4 == 1) {
      if (tag == INT) {
        array.asInstanceOf[Array[Int]](idx).toLong
      } else { // BOOLEAN
        if (array.asInstanceOf[Array[Boolean]](idx)) 1 else 0
      }
    } else {
      if (tag == FLOAT) {
        java.lang.Float.floatToRawIntBits(array.asInstanceOf[Array[Float]](idx)).toLong
      } else { // CHAR
        array.asInstanceOf[Array[Char]](idx).toLong
      }
    }
  }
}

@inline final def mbarray_apply_box[T](array: Any, idx: Int, tag: Tag): T = ???

@inline final def mbarray_update_minibox(array: Any, idx: Int, value: Minibox, tag: Tag): Unit = {
  if (tag % 2 == 0) {
    if (tag % 4 == 0) {
      if (tag == DOUBLE) {
        array.asInstanceOf[Array[Double]](idx) = java.lang.Double.longBitsToDouble(value)
      } else if (tag == SHORT) {
        array.asInstanceOf[Array[Short]](idx) = value.toShort
      } else // UNIT
        array.asInstanceOf[Array[Unit]](idx) = ()
    } else {
      if (tag == LONG) {
        array.asInstanceOf[Array[Long]](idx) = value
      } else // BYTE
        array.asInstanceOf[Array[Byte]](idx) = value.toByte
    }
  } else {
    if (tag % 4 == 1) {
      if (tag == INT) {
        array.asInstanceOf[Array[Int]](idx) = value.toInt
      } else { // BOOLEAN
        array.asInstanceOf[Array[Boolean]](idx) = if (value == 0) false else true
      }
    } else {
      if (tag == FLOAT) {
        array.asInstanceOf[Array[Float]](idx) = java.lang.Float.intBitsToFloat(value.toInt)
      } else { // CHAR
        array.asInstanceOf[Array[Char]](idx) = value.toChar
      }
    }
  }
}

@inline final def mbarray_update_box[T](array: Any, idx: Int, value: T, tag: Tag): Unit = ???

@inline final def mbarray_length(array: Any, tag: Tag): Int = {
  if (tag % 2 == 0) {
    if (tag % 4 == 0) {
      if (tag == DOUBLE) {
        array.asInstanceOf[Array[Double]].length
      } else if (tag == SHORT) {
        array.asInstanceOf[Array[Short]].length
      } else // UNIT
        array.asInstanceOf[Array[Unit]].length
    } else {
      if (tag == LONG) {
        array.asInstanceOf[Array[Long]].length
      } else // BYTE
        array.asInstanceOf[Array[Byte]].length
    }
  } else {
    if (tag % 4 == 1) {
      if (tag == INT) {
        array.asInstanceOf[Array[Int]].length
      } else { // BOOLEAN
        array.asInstanceOf[Array[Boolean]].length
      }
    } else {
      if (tag == FLOAT) {
        array.asInstanceOf[Array[Float]].length
      } else { // CHAR
          array.asInstanceOf[Array[Char]].length
        }
      }
    }
  }
}

/**********************************************************************************************************************\
\**********************************************************************************************************************/

object MiniboxArray_Linear {
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
    if (tag == INT)          { new Array[Int](len) }
    else if (tag == LONG)    { new Array[Long](len) }
    else if (tag == DOUBLE)  { new Array[Double](len) }
    else if (tag == FLOAT)   { new Array[Float](len) }
    else if (tag == CHAR)    { new Array[Char](len) }
    else if (tag == BYTE)    { new Array[Byte](len) }
    else if (tag == SHORT)   { new Array[Short](len) }
    else if (tag == BOOLEAN) { new Array[Boolean](len) }
    else if (tag == UNIT)    { new Array[Unit](len) }
    else ???

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

  @inline final def mbarray_apply_box[T](array: Any, idx: Int, tag: Tag): T = ???

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

  @inline final def mbarray_update_box[T](array: Any, idx: Int, value: T, tag: Tag): Unit = ???

  @inline final def mbarray_length(array: Any, tag: Tag): Int =
    if (tag == INT)          array.asInstanceOf[Array[Int]].length
    else if (tag == LONG)    array.asInstanceOf[Array[Long]].length
    else if (tag == DOUBLE)  array.asInstanceOf[Array[Double]].length
    else if (tag == FLOAT)   array.asInstanceOf[Array[Float]].length
    else if (tag == CHAR)    array.asInstanceOf[Array[Char]].length
    else if (tag == BYTE)    array.asInstanceOf[Array[Byte]].length
    else if (tag == SHORT)   array.asInstanceOf[Array[Short]].length
    else if (tag == BOOLEAN) array.asInstanceOf[Array[Boolean]].length
    else if (tag == UNIT)    array.asInstanceOf[Array[Unit]].length
    else ???
}
