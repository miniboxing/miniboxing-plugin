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

trait SpecializedFractional[@specialized T] extends SpecializedNumeric[T] {
  val extractFractional: Fractional[T]
  
  def div(x: T, y: T): T

  class FractionalOps(lhs: T) extends Ops(lhs) {
    def /(rhs: T) = div(lhs, rhs)
  }
  override implicit def mkNumericOps(lhs: T): FractionalOps =
    new FractionalOps(lhs)
}

object SpecializedFractional {
  trait ExtraImplicits {
    implicit def infixFractionalOps[@specialized T](x: T)(implicit num: SpecializedFractional[T]): SpecializedFractional[T]#FractionalOps = new num.FractionalOps(x)
  }
  object Implicits extends ExtraImplicits
}
