//
//     _____   .__         .__ ___.                    .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_ |__    ____  ___  ___|__|  ____     ____
//   /  \ /  \ |  | /    \ |  | | __ \  /  _ \ \  \/  /|  | /    \   / ___\
//  /    Y    \|  ||   |  \|  | | \_\ \(  <_> ) >    < |  ||   |  \ / /_/  >
//  \____|__  /|__||___|  /|__| |___  / \____/ /__/\_ \|__||___|  / \___  /
//          \/          \/          \/               \/         \/ /_____/
// Copyright (c) 2011-2015 Scala Team, École polytechnique fédérale de Lausanne
//
// Authors:
//    * Milos Stojanovic
//
package scala

import scala.language.implicitConversions

object MiniboxedNumeric {
  trait ExtraImplicits {
    implicit def infixNumericOps[@miniboxed T](x: T)(implicit num: MiniboxedNumeric[T]): MiniboxedNumeric[T]#Ops = new num.Ops(x)
  }
  object Implicits extends ExtraImplicits { }

  trait IntIsMbIntegral extends MiniboxedIntegral[Int] {
    val extractNumeric: Numeric[Int] = Numeric.IntIsIntegral
    val extractIntegral: Integral[Int] = Numeric.IntIsIntegral
    def plus(x: Int, y: Int): Int = x + y
    def minus(x: Int, y: Int): Int = x - y
    def times(x: Int, y: Int): Int = x * y
    def quot(x: Int, y: Int): Int = x / y
    def rem(x: Int, y: Int): Int = x % y
    def negate(x: Int): Int = -x
    def fromInt(x: Int): Int = x
    def toInt(x: Int): Int = x
    def toLong(x: Int): Long = x.toLong
    def toFloat(x: Int): Float = x.toFloat
    def toDouble(x: Int): Double = x.toDouble
  }
  object IntIsMbIntegral extends IntIsMbIntegral with MiniboxedOrdering.IntMbOrdering

  trait ShortIsMbIntegral extends MiniboxedIntegral[Short] {
    val extractNumeric: Numeric[Short] = Numeric.ShortIsIntegral
    val extractIntegral: Integral[Short] = Numeric.ShortIsIntegral
    def plus(x: Short, y: Short): Short = (x + y).toShort
    def minus(x: Short, y: Short): Short = (x - y).toShort
    def times(x: Short, y: Short): Short = (x * y).toShort
    def quot(x: Short, y: Short): Short = (x / y).toShort
    def rem(x: Short, y: Short): Short = (x % y).toShort
    def negate(x: Short): Short = (-x).toShort
    def fromInt(x: Int): Short = x.toShort
    def toInt(x: Short): Int = x.toInt
    def toLong(x: Short): Long = x.toLong
    def toFloat(x: Short): Float = x.toFloat
    def toDouble(x: Short): Double = x.toDouble
  }
  object ShortIsMbIntegral extends ShortIsMbIntegral with MiniboxedOrdering.ShortMbOrdering

  trait ByteIsMbIntegral extends MiniboxedIntegral[Byte] {
    val extractNumeric: Numeric[Byte] = Numeric.ByteIsIntegral
    val extractIntegral: Integral[Byte] = Numeric.ByteIsIntegral
    def plus(x: Byte, y: Byte): Byte = (x + y).toByte
    def minus(x: Byte, y: Byte): Byte = (x - y).toByte
    def times(x: Byte, y: Byte): Byte = (x * y).toByte
    def quot(x: Byte, y: Byte): Byte = (x / y).toByte
    def rem(x: Byte, y: Byte): Byte = (x % y).toByte
    def negate(x: Byte): Byte = (-x).toByte
    def fromInt(x: Int): Byte = x.toByte
    def toInt(x: Byte): Int = x.toInt
    def toLong(x: Byte): Long = x.toLong
    def toFloat(x: Byte): Float = x.toFloat
    def toDouble(x: Byte): Double = x.toDouble
  }
  object ByteIsMbIntegral extends ByteIsMbIntegral with MiniboxedOrdering.ByteMbOrdering

  trait CharIsMbIntegral extends MiniboxedIntegral[Char] {
    val extractNumeric: Numeric[Char] = Numeric.CharIsIntegral
    val extractIntegral: Integral[Char] = Numeric.CharIsIntegral
    def plus(x: Char, y: Char): Char = (x + y).toChar
    def minus(x: Char, y: Char): Char = (x - y).toChar
    def times(x: Char, y: Char): Char = (x * y).toChar
    def quot(x: Char, y: Char): Char = (x / y).toChar
    def rem(x: Char, y: Char): Char = (x % y).toChar
    def negate(x: Char): Char = (-x).toChar
    def fromInt(x: Int): Char = x.toChar
    def toInt(x: Char): Int = x.toInt
    def toLong(x: Char): Long = x.toLong
    def toFloat(x: Char): Float = x.toFloat
    def toDouble(x: Char): Double = x.toDouble
  }
  object CharIsMbIntegral extends CharIsMbIntegral with MiniboxedOrdering.CharMbOrdering

  trait LongIsMbIntegral extends MiniboxedIntegral[Long] {
    val extractNumeric: Numeric[Long] = Numeric.LongIsIntegral
    val extractIntegral: Integral[Long] = Numeric.LongIsIntegral
    def plus(x: Long, y: Long): Long = x + y
    def minus(x: Long, y: Long): Long = x - y
    def times(x: Long, y: Long): Long = x * y
    def quot(x: Long, y: Long): Long = x / y
    def rem(x: Long, y: Long): Long = x % y
    def negate(x: Long): Long = -x
    def fromInt(x: Int): Long = x.toLong
    def toInt(x: Long): Int = x.toInt
    def toLong(x: Long): Long = x
    def toFloat(x: Long): Float = x.toFloat
    def toDouble(x: Long): Double = x.toDouble
  }
  object LongIsMbIntegral extends LongIsMbIntegral with MiniboxedOrdering.LongMbOrdering

  trait FloatIsMbConflicted extends MiniboxedNumeric[Float] {
    def plus(x: Float, y: Float): Float = x + y
    def minus(x: Float, y: Float): Float = x - y
    def times(x: Float, y: Float): Float = x * y
    def negate(x: Float): Float = -x
    def fromInt(x: Int): Float = x.toFloat
    def toInt(x: Float): Int = x.toInt
    def toLong(x: Float): Long = x.toLong
    def toFloat(x: Float): Float = x
    def toDouble(x: Float): Double = x.toDouble
    // logic in Numeric base trait mishandles abs(-0.0f)
    override def abs(x: Float): Float = math.abs(x)
  }
  trait FloatIsMbFractional extends FloatIsMbConflicted with MiniboxedFractional[Float] {
    val extractNumeric: Numeric[Float] = Numeric.FloatIsFractional
    val extractFractional: Fractional[Float] = Numeric.FloatIsFractional
    def div(x: Float, y: Float): Float = x / y
  }
  trait FloatAsIfMbIntegral extends FloatIsMbConflicted with MiniboxedIntegral[Float] {
    val extractNumeric: Numeric[Float] = Numeric.FloatAsIfIntegral
    val extractIntegral: Integral[Float] = Numeric.FloatAsIfIntegral
    def quot(x: Float, y: Float): Float = (BigDecimal(x.toDouble) / BigDecimal(y.toDouble)).floatValue
    def rem(x: Float, y: Float): Float = (BigDecimal(x.toDouble) remainder BigDecimal(y.toDouble)).floatValue
  }
  object FloatIsMbFractional extends FloatIsMbFractional with MiniboxedOrdering.FloatMbOrdering
  object FloatAsIfMbIntegral extends FloatAsIfMbIntegral with MiniboxedOrdering.FloatMbOrdering

  trait DoubleIsMbConflicted extends MiniboxedNumeric[Double] {
    def plus(x: Double, y: Double): Double = x + y
    def minus(x: Double, y: Double): Double = x - y
    def times(x: Double, y: Double): Double = x * y
    def negate(x: Double): Double = -x
    def fromInt(x: Int): Double = x.toDouble
    def toInt(x: Double): Int = x.toInt
    def toLong(x: Double): Long = x.toLong
    def toFloat(x: Double): Float = x.toFloat
    def toDouble(x: Double): Double = x
    // logic in Numeric base trait mishandles abs(-0.0)
    override def abs(x: Double): Double = math.abs(x)
  }
  trait DoubleIsMbFractional extends DoubleIsMbConflicted with MiniboxedFractional[Double] {
    val extractNumeric: Numeric[Double] = Numeric.DoubleIsFractional
    val extractFractional: Fractional[Double] = Numeric.DoubleIsFractional
    def div(x: Double, y: Double): Double = x / y
  }
  trait DoubleAsIfMbIntegral extends DoubleIsMbConflicted with MiniboxedIntegral[Double] {
    val extractNumeric: Numeric[Double] = Numeric.DoubleAsIfIntegral
    val extractIntegral: Integral[Double] = Numeric.DoubleAsIfIntegral
    def quot(x: Double, y: Double): Double = (BigDecimal(x) / BigDecimal(y)).doubleValue
    def rem(x: Double, y: Double): Double = (BigDecimal(x) remainder BigDecimal(y)).doubleValue
  }

  object DoubleIsMbFractional extends DoubleIsMbFractional with MiniboxedOrdering.DoubleMbOrdering
  object DoubleAsIfMbIntegral extends DoubleAsIfMbIntegral with MiniboxedOrdering.DoubleMbOrdering

  implicit def createMiniboxedNumeric[T](implicit num: Numeric[T]): MiniboxedNumeric[T]  =
    ((num) match {
      case scala.math.Numeric.IntIsIntegral => IntIsMbIntegral
      case scala.math.Numeric.ShortIsIntegral => ShortIsMbIntegral
      case scala.math.Numeric.ByteIsIntegral => ByteIsMbIntegral
      case scala.math.Numeric.CharIsIntegral => CharIsMbIntegral
      case scala.math.Numeric.LongIsIntegral => LongIsMbIntegral
      case scala.math.Numeric.FloatAsIfIntegral => FloatAsIfMbIntegral
      case scala.math.Numeric.FloatIsFractional => FloatIsMbFractional
      case scala.math.Numeric.DoubleAsIfIntegral => DoubleAsIfMbIntegral
      case scala.math.Numeric.DoubleIsFractional => DoubleIsMbFractional
      case _ =>
        new MiniboxedNumeric[T] {
          override val extractNumeric: Numeric[T] = num
          override def plus(x: T, y: T): T = num.plus(x, y)
          override def minus(x: T, y: T): T = num.minus(x, y)
          override def times(x: T, y: T): T = num.times(x, y)
          override def negate(x: T): T = num.negate(x)
          override def fromInt(x: Int): T = num.fromInt(x)
          override def toInt(x: T): Int = num.toInt(x)
          override def toLong(x: T): Long = num.toLong(x)
          override def toFloat(x: T): Float = num.toFloat(x)
          override def toDouble(x: T): Double = num.toDouble(x)

          override val extractOrdering: Ordering[T] = num
          override def compare(x: T, y: T): Int = num.compare(x, y)
        }
    }).asInstanceOf[MiniboxedNumeric[T]]
}


trait MiniboxedNumeric[@miniboxed T] extends MiniboxedOrdering[T] {
  val extractNumeric: Numeric[T]

  def plus(x: T, y: T): T
  def minus(x: T, y: T): T
  def times(x: T, y: T): T
  def negate(x: T): T
  def fromInt(x: Int): T
  def toInt(x: T): Int
  def toLong(x: T): Long
  def toFloat(x: T): Float
  def toDouble(x: T): Double

  def zero = fromInt(0)
  def one = fromInt(1)

  def abs(x: T): T = if (lt(x, zero)) negate(x) else x
  def signum(x: T): Int =
    if (lt(x, zero)) -1
    else if (gt(x, zero)) 1
    else 0

  class Ops(lhs: T) {
    def +(rhs: T) = plus(lhs, rhs)
    def -(rhs: T) = minus(lhs, rhs)
    def *(rhs: T) = times(lhs, rhs)
    def unary_-() = negate(lhs)
    def abs(): T = MiniboxedNumeric.this.abs(lhs)
    def signum(): Int = MiniboxedNumeric.this.signum(lhs)
    def toInt(): Int = MiniboxedNumeric.this.toInt(lhs)
    def toLong(): Long = MiniboxedNumeric.this.toLong(lhs)
    def toFloat(): Float = MiniboxedNumeric.this.toFloat(lhs)
    def toDouble(): Double = MiniboxedNumeric.this.toDouble(lhs)
  }
  implicit def mkNumericOps(lhs: T): Ops = new Ops(lhs)
}
