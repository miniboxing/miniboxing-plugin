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

import java.util.Comparator
import scala.language.{implicitConversions, higherKinds}
import scala.math.Ordering.DoubleOrdering

trait SpecializedOrdering[@specialized T] extends Serializable {
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
  
  def reverse: SpecializedOrdering[T] = new SpecializedOrdering[T] {
    override def reverse = outer
    override val extractOrdering: Ordering[T] = new Ordering[T] {
      override def compare(x: T, y: T) = outer.compare(y, x)
    }
    override def compare(x: T, y: T) = outer.compare(y, x)
  }
  
  def on[@specialized U](f: U => T): SpecializedOrdering[U] = new SpecializedOrdering[U] {
    override val extractOrdering: Ordering[U] = new Ordering[U] {
      override def compare(x: U, y: U) = outer.compare(f(x), f(y))
    }
    override def compare(x: U, y: U) = outer.compare(f(x), f(y))
  }
  
  class SpecializedOps(lhs: T) {
    def <(rhs: T) = lt(lhs, rhs)
    def <=(rhs: T) = lteq(lhs, rhs)
    def >(rhs: T) = gt(lhs, rhs)
    def >=(rhs: T) = gteq(lhs, rhs)
    def equiv(rhs: T) = SpecializedOrdering.this.equiv(lhs, rhs)
    def max(rhs: T): T = SpecializedOrdering.this.max(lhs, rhs)
    def min(rhs: T): T = SpecializedOrdering.this.min(lhs, rhs)
  }
  
  implicit def mkOrderingOps(lhs: T): SpecializedOps = new SpecializedOps(lhs)
} 


trait LowPriorityMbOrderingImplicits {
  implicit def ordered[@specialized A <% Comparable[A]]: SpecializedOrdering[A] = new SpecializedOrdering[A] {
    override val extractOrdering: Ordering[A] = new Ordering[A] {
      override def compare(x: A, y: A): Int = x compareTo y
    }
    override def compare(x: A, y: A): Int = x compareTo y
  }
  implicit def comparatorToOrdering[@specialized A](implicit cmp: Comparator[A]): SpecializedOrdering[A] = new SpecializedOrdering[A] {
    override val extractOrdering: Ordering[A] = new Ordering[A] {
      override def compare(x: A, y: A) = cmp.compare(x, y)
    }
    override def compare(x: A, y: A) = cmp.compare(x, y)
  }
}


object SpecializedOrdering extends LowPriorityMbOrderingImplicits {
  def apply[@specialized T](implicit ord: SpecializedOrdering[T]) = ord

  trait ExtraImplicits {
    implicit def seqDerivedOrdering[@specialized CC[X] <: scala.collection.Seq[X], @specialized T](implicit ord: SpecializedOrdering[T]): SpecializedOrdering[CC[T]] =
      new SpecializedOrdering[CC[T]] {
        override val extractOrdering: Ordering[CC[T]] = new Ordering[CC[T]] {
          override def compare(x: CC[T], y: CC[T]): Int = {
            val xe = x.iterator
            val ye = y.iterator
  
            while (xe.hasNext && ye.hasNext) {
              val res = ord.compare(xe.next(), ye.next())
              if (res != 0) return res
            }
  
            SpecializedOrdering.Boolean.compare(xe.hasNext, ye.hasNext)
          }
        }
        override def compare(x: CC[T], y: CC[T]): Int = {
          val xe = x.iterator
          val ye = y.iterator

          while (xe.hasNext && ye.hasNext) {
            val res = ord.compare(xe.next(), ye.next())
            if (res != 0) return res
          }

          SpecializedOrdering.Boolean.compare(xe.hasNext, ye.hasNext)
        }
      }

    implicit def infixOrderingOps[@specialized T](x: T)(implicit ord: SpecializedOrdering[T]): SpecializedOrdering[T]#SpecializedOps = new ord.SpecializedOps(x)
  }

  object Implicits extends ExtraImplicits { }

  def fromLessThan[@specialized T](cmp: (T, T) => Boolean): SpecializedOrdering[T] = new SpecializedOrdering[T] {
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
  
  def by[@specialized T, @specialized S](f: T => S)(implicit ord: SpecializedOrdering[S]): SpecializedOrdering[T] =
    fromLessThan((x, y) => ord.lt(f(x), f(y)))

  trait UnitMbOrdering extends SpecializedOrdering[Unit] {
    override val extractOrdering: Ordering[Unit] = Ordering.Unit
    override def compare(x: Unit, y: Unit) = 0
  }
  implicit object Unit extends UnitMbOrdering

  trait BooleanMbOrdering extends SpecializedOrdering[Boolean] {
    override val extractOrdering: Ordering[Boolean] = Ordering.Boolean
    override def compare(x: Boolean, y: Boolean) = (x, y) match {
      case (false, true) => -1
      case (true, false) => 1
      case _ => 0
    }
  }
  implicit object Boolean extends BooleanMbOrdering

  trait ByteMbOrdering extends SpecializedOrdering[Byte] {
    override val extractOrdering: Ordering[Byte] = Ordering.Byte
    override def compare(x: Byte, y: Byte) = x.toInt - y.toInt
  }
  implicit object Byte extends ByteMbOrdering

  trait CharMbOrdering extends SpecializedOrdering[Char] {
    override val extractOrdering: Ordering[Char] = Ordering.Char
    override def compare(x: Char, y: Char) = x.toInt - y.toInt
  }
  implicit object Char extends CharMbOrdering

  trait ShortMbOrdering extends SpecializedOrdering[Short] {
    override val extractOrdering: Ordering[Short] = Ordering.Short
    override def compare(x: Short, y: Short) = x.toInt - y.toInt
  }
  implicit object Short extends ShortMbOrdering

  trait IntMbOrdering extends SpecializedOrdering[Int] {
    override val extractOrdering: Ordering[Int] = Ordering.Int
    override def compare(x: Int, y: Int) =
      if (x < y) -1
      else if (x == y) 0
      else 1
  }
  implicit object Int extends IntMbOrdering

  trait LongMbOrdering extends SpecializedOrdering[Long] {
    override val extractOrdering: Ordering[Long] = Ordering.Long
    override def compare(x: Long, y: Long) =
      if (x < y) -1
      else if (x == y) 0
      else 1
  }
  implicit object Long extends LongMbOrdering

  trait FloatMbOrdering extends SpecializedOrdering[Float] {
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

    override def reverse: SpecializedOrdering[Float] = new FloatMbOrdering {
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

  trait DoubleMbOrdering extends SpecializedOrdering[Double] {
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

    override def reverse: SpecializedOrdering[Double] = new DoubleMbOrdering {
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

//  implicit def Tuple2[@specialized T1, @specialized T2](implicit ord1: SpecializedOrdering[T1], ord2: SpecializedOrdering[T2]): SpecializedOrdering[(T1, T2)] =
//    new SpecializedOrdering[(T1, T2)]{
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
