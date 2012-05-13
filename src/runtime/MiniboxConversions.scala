package runtime

import java.lang.Double._

object MiniboxConversions {
  import MiniboxTypes._
  /*
   * Conversions to and from Miniboxed format and the actual type
   */
  @inline final def MiniboxToBoolean(l: Minibox): Boolean = (l.toInt != 0)
  @inline final def MiniboxToUnit(l: Minibox): Unit = ()
  @inline final def MiniboxToInt(l: Minibox): Int = l.toInt
  @inline final def MiniboxToDouble(l: Minibox): Double = longBitsToDouble(l)
  
  @inline final def BooleanToMinibox(b: Boolean): Minibox = if (b) 1 else 0
  @inline final def UnitToMinibox(u: Unit): Minibox = 0
  @inline final def IntToMinibox(i: Int): Minibox = i.toLong
  @inline final def DoubleToMinibox(d: Double): Minibox = doubleToLongBits(d)

  
  /**
   * If code like the one below:
   *  class A[T](t: T) {
   *    override def toString = "A" + t
   *  }
   * in the specializer, we have to insert a box operation which takes into 
   * account the type-tag of 't'. Normally, such an operation has to be 
   * inserted during erasure, but we don't want to touch it. 
   * 
   * As a workaround, in our test examples we manually insert MiniboxToAny 
   * in such places. 
   */
  @inline final def minibox2box[T](l: Long, tag: Byte) : T = {
    null.asInstanceOf[T] // dispatch on tag
  }
  @inline final def minibox2box[T](l: Long) : T = {
    null.asInstanceOf[T] // dispatch on tag
  }
  
  // the tag should be known from the context
  @inline final def box2minibox(a: Any): Long = {
    null.asInstanceOf[Long]
  }
  
  
  
}