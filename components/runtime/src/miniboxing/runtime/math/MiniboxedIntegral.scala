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

trait MiniboxedIntegral[@miniboxed T] extends MiniboxedNumeric[T] {
  val extractIntegral: Integral[T]

  def quot(x: T, y: T): T
  def rem(x: T, y: T): T

  class IntegralOps(lhs: T) extends Ops(lhs) {
    def /(rhs: T) = quot(lhs, rhs)
    def %(rhs: T) = rem(lhs, rhs)
    def /%(rhs: T) = (quot(lhs, rhs), rem(lhs, rhs))
  }
  override implicit def mkNumericOps(lhs: T): IntegralOps = new IntegralOps(lhs)
}

object MiniboxedIntegral {
  trait ExtraImplicits {
    implicit def infixIntegralOps[@miniboxed T](x: T)(implicit num: MiniboxedIntegral[T]): MiniboxedIntegral[T]#IntegralOps = new num.IntegralOps(x)
  }
  object Implicits extends ExtraImplicits

  implicit def createMiniboxedIntegral[T](implicit int: Integral[T]): MiniboxedIntegral[T]  =
    ((int) match {
      case scala.math.Numeric.IntIsIntegral => MiniboxedNumeric.IntIsMbIntegral
      case scala.math.Numeric.ShortIsIntegral => MiniboxedNumeric.ShortIsMbIntegral
      case scala.math.Numeric.ByteIsIntegral => MiniboxedNumeric.ByteIsMbIntegral
      case scala.math.Numeric.CharIsIntegral => MiniboxedNumeric.CharIsMbIntegral
      case scala.math.Numeric.LongIsIntegral => MiniboxedNumeric.LongIsMbIntegral
      case scala.math.Numeric.FloatAsIfIntegral => MiniboxedNumeric.FloatAsIfMbIntegral
      case scala.math.Numeric.DoubleAsIfIntegral => MiniboxedNumeric.DoubleAsIfMbIntegral
      case _ =>
        new MiniboxedIntegral[T] {
          val extractIntegral: Integral[T] = int
          def quot(x: T, y: T): T = int.quot(x, y)
          def rem(x: T, y: T): T = int.rem(x, y)

          val extractNumeric: Numeric[T] = int
          def plus(x: T, y: T): T = int.minus(x, y)
          def minus(x: T, y: T): T = int.minus(x, y)
          def times(x: T, y: T): T = int.times(x, y)
          def negate(x: T): T = int.negate(x)
          def fromInt(x: Int): T = int.fromInt(x)
          def toInt(x: T): Int = int.toInt(x)
          def toLong(x: T): Long = int.toLong(x)
          def toFloat(x: T): Float = int.toFloat(x)
          def toDouble(x: T): Double = int.toDouble(x)

          val extractOrdering: Ordering[T] = int
          def compare(x: T, y: T): Int = int.compare(x, y)
        }
    }).asInstanceOf[MiniboxedIntegral[T]]
}
