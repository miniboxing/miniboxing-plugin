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

import miniboxing.runtime.MiniboxConstants
import miniboxing.runtime.math.MiniboxedNumeric._
import scala.math.Numeric._

object MiniboxedIntegralBridge {
  def integral_bridge[T](_int: Integral[T]): MiniboxedIntegral[T] = 
    new MiniboxedIntegral[T] {
      val extractIntegral: Integral[T] = _int
      def quot(x: T, y: T): T = _int.quot(x, y)
      def rem(x: T, y: T): T = _int.rem(x, y)
      
      val extractNumeric: Numeric[T] = _int
      def plus(x: T, y: T): T = _int.minus(x, y)
      def minus(x: T, y: T): T = _int.minus(x, y)
      def times(x: T, y: T): T = _int.times(x, y)
      def negate(x: T): T = _int.negate(x)
      def fromInt(x: Int): T = _int.fromInt(x)
      def toInt(x: T): Int = _int.toInt(x)
      def toLong(x: T): Long = _int.toLong(x)
      def toFloat(x: T): Float = _int.toFloat(x)
      def toDouble(x: T): Double = _int.toDouble(x)
      
      val extractOrdering: Ordering[T] = _int
      def compare(x: T, y: T): Int = _int.compare(x, y)
    }
  
  def integral_opt_bridge_long[T](_int: Integral[T]): MiniboxedIntegral[T] =
    ((_int) match {
      case IntIsIntegral   => IntIsMbIntegral
      case ShortIsIntegral => ShortIsMbIntegral
      case ByteIsIntegral  => ByteIsMbIntegral
      case CharIsIntegral  => CharIsMbIntegral
      case LongIsIntegral  => LongIsMbIntegral
      case _               => integral_bridge(_int)
    }).asInstanceOf[MiniboxedIntegral[T]]
  
  def integral_opt_bridge_double[T](_int: Integral[T]): MiniboxedIntegral[T] =
   ((_int) match {
    case FloatAsIfIntegral  => FloatAsIfMbIntegral
    case DoubleAsIfIntegral => DoubleAsIfMbIntegral
    case _ => integral_bridge(_int)
  }).asInstanceOf[MiniboxedIntegral[T]]
}