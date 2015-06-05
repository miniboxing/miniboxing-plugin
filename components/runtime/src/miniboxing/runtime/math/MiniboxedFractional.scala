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

import scala.language.implicitConversions

trait MiniboxedFractional[@miniboxed T] extends MiniboxedNumeric[T] {
  val extractFractional: Fractional[T]

  def div(x: T, y: T): T

  class FractionalOps(lhs: T) extends Ops(lhs) {
    def /(rhs: T) = div(lhs, rhs)
  }
  override implicit def mkNumericOps(lhs: T): FractionalOps =
    new FractionalOps(lhs)
}

object MiniboxedFractional {
  trait ExtraImplicits {
    implicit def infixFractionalOps[@miniboxed T](x: T)(implicit num: MiniboxedFractional[T]): MiniboxedFractional[T]#FractionalOps = new num.FractionalOps(x)
  }
  object Implicits extends ExtraImplicits

  implicit def createMiniboxedFractional[T](implicit frac: Fractional[T]): MiniboxedFractional[T]  =
    ((frac) match {
      case scala.math.Numeric.FloatIsFractional => MiniboxedNumeric.FloatIsMbFractional
      case scala.math.Numeric.DoubleIsFractional => MiniboxedNumeric.DoubleIsMbFractional
      case _ =>
        new MiniboxedFractional[T] {
          val extractFractional: Fractional[T] = frac
          def div(x: T, y: T): T = frac.div(x, y)

          val extractNumeric: Numeric[T] = frac
          def plus(x: T, y: T): T = frac.minus(x, y)
          def minus(x: T, y: T): T = frac.minus(x, y)
          def times(x: T, y: T): T = frac.times(x, y)
          def negate(x: T): T = frac.negate(x)
          def fromInt(x: Int): T = frac.fromInt(x)
          def toInt(x: T): Int = frac.toInt(x)
          def toLong(x: T): Long = frac.toLong(x)
          def toFloat(x: T): Float = frac.toFloat(x)
          def toDouble(x: T): Double = frac.toDouble(x)

          val extractOrdering: Ordering[T] = frac
          def compare(x: T, y: T): Int = frac.compare(x, y)
        }
    }).asInstanceOf[MiniboxedFractional[T]]
}
