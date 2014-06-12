package miniboxing.test.compile.bugs


import scala.{miniboxed => spec}

trait OrderProduct2[@miniboxed A,@miniboxed B] extends Order[(A, B)] with EqProduct2[A, B] {
  implicit def structure1: Order[A]
  implicit def structure2: Order[B]
  def compare(x0: (A, B), x1: (A, B)): Int = {
    var cmp: Int = 0
    cmp = structure1.compare(x0._1, x1._1)
    if (cmp != 0) cmp else {
      cmp = structure2.compare(x0._2, x1._2)
      if (cmp != 0) cmp else {
        0
      }
    }
  }
  override def eqv(x0: (A, B), x1: (A, B)): Boolean = super.eqv(x0, x1)
}

trait EqProduct2[@spec A,@spec B] extends Eq[(A, B)] { 
  implicit def structure1: Eq[A] 
  implicit def structure2: Eq[B] 
  def eqv(x0: (A, B), x1: (A, B)): Boolean = structure1.eqv(x0._1, x1._1) && structure2.eqv(x0._2, x1._2) 
}

trait Order[@spec A] extends Eq[A] {
  self =>

  def eqv(x: A, y: A): Boolean = compare(x, y) == 0
  def gt(x: A, y: A): Boolean = compare(x, y) > 0
  def lt(x: A, y: A): Boolean = compare(x, y) < 0
  def gteqv(x: A, y: A): Boolean = compare(x, y) >= 0
  def lteqv(x: A, y: A): Boolean = compare(x, y) <= 0

  def min(x: A, y: A): A = if (lt(x, y)) x else y
  def max(x: A, y: A): A = if (gt(x, y)) x else y
  def compare(x: A, y: A): Int

  override def on[@spec B](f: B => A): Order[B] = new MappedOrder(this)(f)

  def reverse: Order[A] = new ReversedOrder(this)
}

class MappedOrder[@spec A, @spec B](order: Order[B])(f: A => B) extends Order[A] {
  def compare(x: A, y: A) = order.compare(f(x), f(y))
}

class ReversedOrder[@spec A](order: Order[A]) extends Order[A] {
  def compare(x: A, y: A) = order.compare(y, x)
}

object Order {
  @inline final def apply[A](implicit o: Order[A]) = o

  def by[@spec A, @spec B](f: A => B)(implicit o: Order[B]): Order[A] = o.on(f)

  def from[@spec A](f: (A, A) => Int): Order[A] = new Order[A] {
    def compare(x: A, y: A) = f(x, y)
  }

  implicit def ordering[A](implicit o: Order[A]) = new Ordering[A] {
    def compare(x: A, y: A) = o.compare(x, y)
  }
}

/**
 * A type class used to determine equality between 2 instances of the same
 * type. Any 2 instances `x` and `y` are equal if `eqv(x, y)` is `true`.
 * Moreover, `eqv` should form an equivalence relation.
 */
trait Eq[@spec A] {
  /** Returns `true` if `x` and `y` are equivalent, `false` otherwise. */
  def eqv(x:A, y:A): Boolean

  /** Returns `false` if `x` and `y` are equivalent, `true` otherwise. */
  def neqv(x:A, y:A): Boolean = !eqv(x, y)

  /**
   * Constructs a new `Eq` instance for type `B` where 2 elements are
   * equivalent iff `eqv(f(x), f(y))`.
   */
  def on[@spec B](f:B => A): Eq[B] = new MappedEq(this)(f)
}

class MappedEq[@spec A, @spec B](eq: Eq[B])(f: A => B) extends Eq[A] {
  def eqv(x: A, y: A): Boolean = eq.eqv(f(x), f(x))
}

object Eq {
  def apply[A](implicit e:Eq[A]):Eq[A] = e

  def by[@spec A, @spec B](f:A => B)(implicit e:Eq[B]): Eq[A] = new MappedEq(e)(f)
}
