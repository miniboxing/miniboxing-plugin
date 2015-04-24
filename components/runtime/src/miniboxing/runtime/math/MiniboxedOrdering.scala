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

import java.util.Comparator
import scala.language.{implicitConversions, higherKinds}
import scala.math.Ordering.DoubleOrdering

trait MiniboxedOrdering[@miniboxed T] extends Serializable {
  outer => 
    
  val extractOrdering: Ordering[T]
  
  def tryCompare(x: T, y: T) = Some(compare(x, y))
  
  def compare(x: T, y: T): Int
  def lteq(x: T, y: T): Boolean = compare(x, y) <= 0
  def gteq(x: T, y: T): Boolean = compare(x, y) >= 0
  def lt(x: T, y: T): Boolean = compare(x, y) < 0
  def gt(x: T, y: T): Boolean = compare(x, y) > 0
  def equiv(x: T, y: T): Boolean = compare(x, y) == 0
  def max(x: T, y: T): T = if (gteq(x, y)) x else y
  def min(x: T, y: T): T = if (lteq(x, y)) x else y
  
  def reverse: MiniboxedOrdering[T] = new MiniboxedOrdering[T] {
    override def reverse = outer
    override val extractOrdering: Ordering[T] = new Ordering[T] {
      override def compare(x: T, y: T) = outer.compare(y, x)
    }
    override def compare(x: T, y: T) = outer.compare(y, x)
  }
  
  def on[@miniboxed U](f: U => T): MiniboxedOrdering[U] = new MiniboxedOrdering[U] {
    override val extractOrdering: Ordering[U] = new Ordering[U] {
      override def compare(x: U, y: U) = outer.compare(f(x), f(y))
    }
    override def compare(x: U, y: U) = outer.compare(f(x), f(y))
  }
  
  class MiniboxedOps(lhs: T) {
    def <(rhs: T) = lt(lhs, rhs)
    def <=(rhs: T) = lteq(lhs, rhs)
    def >(rhs: T) = gt(lhs, rhs)
    def >=(rhs: T) = gteq(lhs, rhs)
    def equiv(rhs: T) = MiniboxedOrdering.this.equiv(lhs, rhs)
    def max(rhs: T): T = MiniboxedOrdering.this.max(lhs, rhs)
    def min(rhs: T): T = MiniboxedOrdering.this.min(lhs, rhs)
  }
  
  implicit def mkOrderingOps(lhs: T): MiniboxedOps = new MiniboxedOps(lhs)
} 


trait LowPriorityMbOrderingImplicits {
  implicit def ordered[@miniboxed A <% Comparable[A]]: MiniboxedOrdering[A] = new MiniboxedOrdering[A] {
    override val extractOrdering: Ordering[A] = new Ordering[A] {
      override def compare(x: A, y: A): Int = x compareTo y
    }
    override def compare(x: A, y: A): Int = x compareTo y
  }
  implicit def comparatorToOrdering[@miniboxed A](implicit cmp: Comparator[A]): MiniboxedOrdering[A] = new MiniboxedOrdering[A] {
    override val extractOrdering: Ordering[A] = new Ordering[A] {
      override def compare(x: A, y: A) = cmp.compare(x, y)
    }
    override def compare(x: A, y: A) = cmp.compare(x, y)
  }
}


object MiniboxedOrdering extends LowPriorityMbOrderingImplicits {
  def apply[@miniboxed T](implicit ord: MiniboxedOrdering[T]) = ord

  trait ExtraImplicits {
    implicit def seqDerivedOrdering[@miniboxed CC[X] <: scala.collection.Seq[X], @miniboxed T](implicit ord: MiniboxedOrdering[T]): MiniboxedOrdering[CC[T]] =
      new MiniboxedOrdering[CC[T]] {
        override val extractOrdering: Ordering[CC[T]] = new Ordering[CC[T]] {
          override def compare(x: CC[T], y: CC[T]): Int = {
            val xe = x.iterator
            val ye = y.iterator
  
            while (xe.hasNext && ye.hasNext) {
              val res = ord.compare(xe.next(), ye.next())
              if (res != 0) return res
            }
  
            MiniboxedOrdering.Boolean.compare(xe.hasNext, ye.hasNext)
          }
        }
        override def compare(x: CC[T], y: CC[T]): Int = {
          val xe = x.iterator
          val ye = y.iterator

          while (xe.hasNext && ye.hasNext) {
            val res = ord.compare(xe.next(), ye.next())
            if (res != 0) return res
          }

          MiniboxedOrdering.Boolean.compare(xe.hasNext, ye.hasNext)
        }
      }

    implicit def infixOrderingOps[@miniboxed T](x: T)(implicit ord: MiniboxedOrdering[T]): MiniboxedOrdering[T]#MiniboxedOps = new ord.MiniboxedOps(x)
  }

  object Implicits extends ExtraImplicits { }

  def fromLessThan[@miniboxed T](cmp: (T, T) => Boolean): MiniboxedOrdering[T] = new MiniboxedOrdering[T] {
    override val extractOrdering: Ordering[T] = new Ordering[T] {
      override def compare(x: T, y: T): Int = if (cmp(x, y)) -1 else if (cmp(y, x)) 1 else 0
    }
    override def compare(x: T, y: T) = if (cmp(x, y)) -1 else if (cmp(y, x)) 1 else 0
    // overrides to avoid multiple comparisons
    override def lt(x: T, y: T): Boolean = cmp(x, y)
    override def gt(x: T, y: T): Boolean = cmp(y, x)
    override def gteq(x: T, y: T): Boolean = !cmp(x, y)
    override def lteq(x: T, y: T): Boolean = !cmp(y, x)
  }
  
  def by[@miniboxed T, @miniboxed S](f: T => S)(implicit ord: MiniboxedOrdering[S]): MiniboxedOrdering[T] =
    fromLessThan((x, y) => ord.lt(f(x), f(y)))

  trait UnitMbOrdering extends MiniboxedOrdering[Unit] {
    override val extractOrdering: Ordering[Unit] = Ordering.Unit
    override def compare(x: Unit, y: Unit) = 0
  }
  implicit object Unit extends UnitMbOrdering

  trait BooleanMbOrdering extends MiniboxedOrdering[Boolean] {
    override val extractOrdering: Ordering[Boolean] = Ordering.Boolean
    override def compare(x: Boolean, y: Boolean) = (x, y) match {
      case (false, true) => -1
      case (true, false) => 1
      case _ => 0
    }
  }
  implicit object Boolean extends BooleanMbOrdering

  trait ByteMbOrdering extends MiniboxedOrdering[Byte] {
    override val extractOrdering: Ordering[Byte] = Ordering.Byte
    override def compare(x: Byte, y: Byte) = x.toInt - y.toInt
  }
  implicit object Byte extends ByteMbOrdering

  trait CharMbOrdering extends MiniboxedOrdering[Char] {
    override val extractOrdering: Ordering[Char] = Ordering.Char
    override def compare(x: Char, y: Char) = x.toInt - y.toInt
  }
  implicit object Char extends CharMbOrdering

  trait ShortMbOrdering extends MiniboxedOrdering[Short] {
    override val extractOrdering: Ordering[Short] = Ordering.Short
    override def compare(x: Short, y: Short) = x.toInt - y.toInt
  }
  implicit object Short extends ShortMbOrdering

  trait IntMbOrdering extends MiniboxedOrdering[Int] {
    override val extractOrdering: Ordering[Int] = Ordering.Int
    override def compare(x: Int, y: Int) =
      if (x < y) -1
      else if (x == y) 0
      else 1
  }
  implicit object Int extends IntMbOrdering

  trait LongMbOrdering extends MiniboxedOrdering[Long] {
    override val extractOrdering: Ordering[Long] = Ordering.Long
    override def compare(x: Long, y: Long) =
      if (x < y) -1
      else if (x == y) 0
      else 1
  }
  implicit object Long extends LongMbOrdering

  trait FloatMbOrdering extends MiniboxedOrdering[Float] {
    outer =>
      
    override val extractOrdering: Ordering[Float] = Ordering.Float

    override def compare(x: Float, y: Float) = java.lang.Float.compare(x, y)

    override def lteq(x: Float, y: Float): Boolean = x <= y
    override def gteq(x: Float, y: Float): Boolean = x >= y
    override def lt(x: Float, y: Float): Boolean = x < y
    override def gt(x: Float, y: Float): Boolean = x > y
    override def equiv(x: Float, y: Float): Boolean = x == y
    override def max(x: Float, y: Float): Float = math.max(x, y)
    override def min(x: Float, y: Float): Float = math.min(x, y)

    override def reverse: MiniboxedOrdering[Float] = new FloatMbOrdering {
      override def reverse = outer
      override def compare(x: Float, y: Float) = outer.compare(y, x)

      override def lteq(x: Float, y: Float): Boolean = outer.lteq(y, x)
      override def gteq(x: Float, y: Float): Boolean = outer.gteq(y, x)
      override def lt(x: Float, y: Float): Boolean = outer.lt(y, x)
      override def gt(x: Float, y: Float): Boolean = outer.gt(y, x)
      override def min(x: Float, y: Float): Float = outer.max(x, y)
      override def max(x: Float, y: Float): Float = outer.min(x, y)

    }
  }
  implicit object Float extends FloatMbOrdering

  trait DoubleMbOrdering extends MiniboxedOrdering[Double] {
    outer =>
      
    override val extractOrdering: Ordering[Double] = Ordering.Double

    override def compare(x: Double, y: Double) = java.lang.Double.compare(x, y)

    override def lteq(x: Double, y: Double): Boolean = x <= y
    override def gteq(x: Double, y: Double): Boolean = x >= y
    override def lt(x: Double, y: Double): Boolean = x < y
    override def gt(x: Double, y: Double): Boolean = x > y
    override def equiv(x: Double, y: Double): Boolean = x == y
    override def max(x: Double, y: Double): Double = math.max(x, y)
    override def min(x: Double, y: Double): Double = math.min(x, y)

    override def reverse: MiniboxedOrdering[Double] = new DoubleMbOrdering {
      override def reverse = outer
      override def compare(x: Double, y: Double) = outer.compare(y, x)

      override def lteq(x: Double, y: Double): Boolean = outer.lteq(y, x)
      override def gteq(x: Double, y: Double): Boolean = outer.gteq(y, x)
      override def lt(x: Double, y: Double): Boolean = outer.lt(y, x)
      override def gt(x: Double, y: Double): Boolean = outer.gt(y, x)
      override def min(x: Double, y: Double): Double = outer.max(x, y)
      override def max(x: Double, y: Double): Double = outer.min(x, y)
    }
  }
  implicit object Double extends DoubleMbOrdering

//  implicit def Tuple2[@miniboxed T1, @miniboxed T2](implicit ord1: MiniboxedOrdering[T1], ord2: MiniboxedOrdering[T2]): MiniboxedOrdering[(T1, T2)] =
//    new MiniboxedOrdering[(T1, T2)]{
//      override val extractOrdering: Ordering[(T1, T2)] = new Ordering[(T1, T2)] {
//        override def compare(x: (T1, T2), y: (T1, T2)): Int = {
//          val compare1 = ord1.compare(x._1, y._1)
//          if (compare1 != 0) return compare1
//          val compare2 = ord2.compare(x._2, y._2)
//          if (compare2 != 0) return compare2
//          0
//        }
//      }
//      override def compare(x: (T1, T2), y: (T1, T2)): Int = {
//        val compare1 = ord1.compare(x._1, y._1)
//        if (compare1 != 0) return compare1
//        val compare2 = ord2.compare(x._2, y._2)
//        if (compare2 != 0) return compare2
//        0
//      }
//    }
}
