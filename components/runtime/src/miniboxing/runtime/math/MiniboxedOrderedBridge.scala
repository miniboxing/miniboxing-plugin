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

object MiniboxedOrderedBridge {
  
  def ordered_bridge[T](_ord: Ordered[T]): MiniboxedOrdered[T] = 
    new MiniboxedOrdered[T] {
      val extractOrdered: Ordered[T] = _ord
      def compare(that: T): Int = _ord.compare(that)
    }
  
  def ordered_opt_bridge_long[T](T_Tag: Byte, _ord: Ordered[T]): MiniboxedOrdered[T] = 
    ((T_Tag) match {
      case MiniboxConstants.UNIT =>
        val _ord_cast = _ord.asInstanceOf[Ordered[Unit]]
        new MiniboxedOrdered[Unit] {
          val extractOrdered: Ordered[Unit] = _ord_cast
          def compare(that: Unit): Int = _ord_cast.compare(that)
        }
      case MiniboxConstants.BOOLEAN =>
        val _ord_cast = _ord.asInstanceOf[Ordered[Boolean]]
        new MiniboxedOrdered[Boolean] {
          val extractOrdered: Ordered[Boolean] = _ord_cast
          def compare(that: Boolean): Int = _ord_cast.compare(that)
        }
      case MiniboxConstants.BYTE =>
        val _ord_cast = _ord.asInstanceOf[Ordered[Byte]]
        new MiniboxedOrdered[Byte] {
          val extractOrdered: Ordered[Byte] = _ord_cast
          def compare(that: Byte): Int = _ord_cast.compare(that)
        }
      case MiniboxConstants.SHORT =>
        val _ord_cast = _ord.asInstanceOf[Ordered[Short]]
        new MiniboxedOrdered[Short] {
          val extractOrdered: Ordered[Short] = _ord_cast
          def compare(that: Short): Int = _ord_cast.compare(that)
        }
      case MiniboxConstants.CHAR =>
        val _ord_cast = _ord.asInstanceOf[Ordered[Char]]
        new MiniboxedOrdered[Char] {
          val extractOrdered: Ordered[Char] = _ord_cast
          def compare(that: Char): Int = _ord_cast.compare(that)
        }
      case MiniboxConstants.INT =>
        val _ord_cast = _ord.asInstanceOf[Ordered[Int]]
        new MiniboxedOrdered[Int] {
          val extractOrdered: Ordered[Int] = _ord_cast
          def compare(that: Int): Int = _ord_cast.compare(that)
        }
      case MiniboxConstants.LONG =>
        val _ord_cast = _ord.asInstanceOf[Ordered[Long]]
        new MiniboxedOrdered[Long] {
          val extractOrdered: Ordered[Long] = _ord_cast
          def compare(that: Long): Int = _ord_cast.compare(that)
        }
      case _ =>
        ordered_bridge(_ord)
    }).asInstanceOf[MiniboxedOrdered[T]]
  
  def ordered_opt_bridge_double[T](T_Tag: Byte, _ord: Ordered[T]): MiniboxedOrdered[T] =
    ((T_Tag) match {
      case MiniboxConstants.FLOAT =>
        val _ord_cast = _ord.asInstanceOf[Ordered[Float]]
        new MiniboxedOrdered[Float] {
          val extractOrdered: Ordered[Float] = _ord_cast
          def compare(that: Float): Int = _ord_cast.compare(that)
        }
      case MiniboxConstants.DOUBLE =>
        val _ord_cast = _ord.asInstanceOf[Ordered[Double]]
        new MiniboxedOrdered[Double] {
          val extractOrdered: Ordered[Double] = _ord_cast
          def compare(that: Double): Int = _ord_cast.compare(that)
        }
      case _ =>
        ordered_bridge(_ord)
    }).asInstanceOf[MiniboxedOrdered[T]]
}
