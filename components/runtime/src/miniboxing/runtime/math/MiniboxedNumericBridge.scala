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
package miniboxing.runtime.math

import miniboxing.runtime.math.MiniboxedNumeric._
import scala.math.Numeric._

object MiniboxedNumericBridge {
  def numeric_bridge[T](_num: Numeric[T]): MiniboxedNumeric[T] = 
    new MiniboxedNumeric[T] {
      override val extractNumeric: Numeric[T] = _num
      override def plus(x: T, y: T): T = _num.plus(x, y)
      override def minus(x: T, y: T): T = _num.minus(x, y)
      override def times(x: T, y: T): T = _num.times(x, y)
      override def negate(x: T): T = _num.negate(x)
      override def fromInt(x: Int): T = _num.fromInt(x)
      override def toInt(x: T): Int = _num.toInt(x)
      override def toLong(x: T): Long = _num.toLong(x)
      override def toFloat(x: T): Float = _num.toFloat(x)
      override def toDouble(x: T): Double = _num.toDouble(x)
      
      override val extractOrdering: Ordering[T] = _num
      override def compare(x: T, y: T): Int = _num.compare(x, y)
    }
  
  def numeric_opt_bridge_long[T](_num: Numeric[T]): MiniboxedNumeric[T] =
    ((_num) match {
      case IntIsIntegral => IntIsMbIntegral
      case ShortIsIntegral => ShortIsMbIntegral
      case ByteIsIntegral => ByteIsMbIntegral
      case CharIsIntegral => CharIsMbIntegral
      case LongIsIntegral => LongIsMbIntegral
      case _ => numeric_bridge(_num)
    }).asInstanceOf[MiniboxedNumeric[T]]
  
  def numeric_opt_bridge_double[T](_num: Numeric[T]): MiniboxedNumeric[T] = 
   ((_num) match {
      case FloatAsIfIntegral => FloatAsIfMbIntegral
      case FloatIsFractional => FloatIsMbFractional
      case DoubleAsIfIntegral => DoubleAsIfMbIntegral
      case DoubleIsFractional => DoubleIsMbFractional
      case _ => numeric_bridge(_num)
    }).asInstanceOf[MiniboxedNumeric[T]]
}
