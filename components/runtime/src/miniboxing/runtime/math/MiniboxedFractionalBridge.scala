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

object MiniboxedFractionalBridge {
  def fractional_bridge[T](_frac: Fractional[T]): MiniboxedFractional[T] = 
    new MiniboxedFractional[T] {
      val extractFractional: Fractional[T] = _frac
      def div(x: T, y: T): T = _frac.div(x, y)
      
      val extractNumeric: Numeric[T] = _frac
      def plus(x: T, y: T): T = _frac.minus(x, y)
      def minus(x: T, y: T): T = _frac.minus(x, y)
      def times(x: T, y: T): T = _frac.times(x, y)
      def negate(x: T): T = _frac.negate(x)
      def fromInt(x: Int): T = _frac.fromInt(x)
      def toInt(x: T): Int = _frac.toInt(x)
      def toLong(x: T): Long = _frac.toLong(x)
      def toFloat(x: T): Float = _frac.toFloat(x)
      def toDouble(x: T): Double = _frac.toDouble(x)
      
      val extractOrdering: Ordering[T] = _frac
      def compare(x: T, y: T): Int = _frac.compare(x, y)
    }

  def fractional_opt_bridge_double[T](_frac: Fractional[T]): MiniboxedFractional[T] =
   ((_frac) match {
    case FloatIsFractional  => FloatIsMbFractional
    case DoubleIsFractional => DoubleIsMbFractional
    case _ => fractional_bridge(_frac)
  }).asInstanceOf[MiniboxedFractional[T]]
}