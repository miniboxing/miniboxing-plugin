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
package miniboxing.benchmarks.numerics.specialized.math

import scala.language.implicitConversions

trait SpecializedIntegral[@specialized T] extends SpecializedNumeric[T] {
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

object SpecializedIntegral {
  trait ExtraImplicits {
    implicit def infixIntegralOps[@specialized T](x: T)(implicit num: SpecializedIntegral[T]): SpecializedIntegral[T]#IntegralOps = new num.IntegralOps(x)
  }
  object Implicits extends ExtraImplicits
}
