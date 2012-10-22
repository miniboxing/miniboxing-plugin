package miniboxing.runtime

object MiniboxArray {
  import MiniboxTypes._
  import MiniboxConstants._

  @inline final def newArray[T](len: Int): Array[T] = 
    sys.error("I'm just a compile time entity")
    
  @inline final def internal_newArray(len: Int, tag: Tag): Any = {
    tag match {
      case UNIT =>
        new Array[Unit](len)
      case BOOLEAN =>
        new Array[Boolean](len)
      case BYTE =>
        new Array[Byte](len)
      case CHAR =>
        new Array[Char](len)
      case SHORT =>
        new Array[Short](len)
      case INT =>
        new Array[Int](len)
      case LONG =>
        new Array[Long](len)
      case FLOAT =>
        new Array[Float](len)
      case DOUBLE =>
        new Array[Double](len)
    }
  }

}