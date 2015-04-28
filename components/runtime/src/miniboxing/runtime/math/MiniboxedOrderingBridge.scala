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

import scala.Ordering

object MiniboxedOrderingBridge {
  
  def ordering_bridge[T](_ord: Ordering[T]): MiniboxedOrdering[T] =
  	new MiniboxedOrdering[T] {
      val extractOrdering: Ordering[T] = _ord
      def compare(x: T, y: T): Int = _ord.compare(x, y)
    }
  
  def ordering_opt_bridge_long[T](_ord: Ordering[T]): MiniboxedOrdering[T] =
    ((_ord) match {
      case Ordering.Unit => MiniboxedOrdering.Unit
      case Ordering.Boolean => MiniboxedOrdering.Boolean
      case Ordering.Byte => MiniboxedOrdering.Byte
      case Ordering.Short => MiniboxedOrdering.Short
      case Ordering.Char => MiniboxedOrdering.Char
      case Ordering.Int => MiniboxedOrdering.Int
      case Ordering.Long => MiniboxedOrdering.Long
      case _ => ordering_bridge(_ord)
    }).asInstanceOf[MiniboxedOrdering[T]]
  
  def ordering_opt_bridge_double[T](_ord: Ordering[T]): MiniboxedOrdering[T] = 
   ((_ord) match {
    case Ordering.Float => MiniboxedOrdering.Float
    case Ordering.Double => MiniboxedOrdering.Double
    case _ => ordering_bridge(_ord)
  }).asInstanceOf[MiniboxedOrdering[T]]
}
