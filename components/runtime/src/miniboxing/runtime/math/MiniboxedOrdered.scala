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

trait MiniboxedOrdered[@miniboxed T] {
  outer => 
    
  val extractOrdered: Ordered[T]
  
  def compare(that: T): Int
  def <  (that: T): Boolean = (this compare that) <  0
  def >  (that: T): Boolean = (this compare that) >  0
  def <= (that: T): Boolean = (this compare that) <= 0
  def >= (that: T): Boolean = (this compare that) >= 0
  def compareTo(that: T): Int = compare(that)
}

object MiniboxedOrdered {
  implicit def orderingToOrdered[@miniboxed T](x: T)(implicit ord: MiniboxedOrdering[T]): MiniboxedOrdered[T] =
    new MiniboxedOrdered[T] { 
      val extractOrdered: Ordered[T] = Ordered.orderingToOrdered[T](x)(ord.extractOrdering)
      def compare(that: T): Int = ord.compare(x, that) 
    }
}
