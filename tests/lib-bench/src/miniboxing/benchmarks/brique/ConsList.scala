package brique

// import algebra.{Eq, Monoid} <= spire is not miniboxed, this could produce slowdowns
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.{Boolean, Int, List, None, Nothing, Option, Some}
import scala.{inline, miniboxed, unchecked}

/**
 * Purely functional single linked list
 * [[ConsList]] is inspired by scalaz.IList
 */
sealed abstract class ConsList[@miniboxed A] extends scala.Product with scala.Serializable {
  import brique.ConsList._

  /** add an element to the back */
  final def append(a: A): ConsList[A] =
    reverse.foldLeft(ConsList.singleton(a))((acc, a) => Cons(a, acc))

  /** add a [[ConsList]] to the back */
  final def concat(as: ConsList[A]): ConsList[A] =
    reverse.foldLeft(as)((acc, a) => Cons(a, acc))

  /** alias for concat */
  final def ++(as: ConsList[A]): ConsList[A] =
    reverse.foldLeft(as)((acc, a) => Cons(a, acc))

  /** drop the `n` first elements */
  final def drop(n: Int): ConsList[A] = {
    var acc = this
    var m = n
    while(true){
      acc match {
        case CNil() => return acc
        case Cons(h, t) =>
          if(m > 0) { acc = t; m = m - 1 }
          else return acc
      }
    }
    acc
  }

  /** drop elements as long as the predicate holds */
  final def dropWhile(p: A => Boolean): ConsList[A] = {
    var acc = this
    while(true){
      acc match {
        case CNil() => return acc
        case Cons(h, t) =>
          if(p(h)) acc = t
          else return acc
      }
    }
    acc
  }

  /** filter all elements that match the predicate */
  final def filter(p: A => Boolean): ConsList[A] = {
    var acc = empty[A]
    var l = this
    while(true){
      l match {
        case Cons(h, t) =>
          if(p(h)) acc = Cons(h, acc)
          l = t
        case CNil() => return acc.reverse
      }
    }
    acc
  }

  final def flatMap[@miniboxed B](f: A => ConsList[B]): ConsList[B] =
    reverse.foldLeft(empty[B])((acc, a) => f(a) ++ acc )

  final def foldLeft[@miniboxed B](b: B)(f: (B, A) => B): B = {
    var acc = b
    var l = this
    while(true){
      l match {
        case Cons(h, t) =>
          acc = f(acc, h)
          l = t
        case CNil() => return acc
      }
    }
    acc
  }

//  final def foldMap[@miniboxed B](b: B)(f: A => B)(implicit B: Monoid[B]): B =
//    reverse.foldLeft(b)((acc, a) => B.combine(f(a), acc))

  final def foldRight[@miniboxed B](b: B)(f: (A, B) => B): B =
    reverse.foldLeft(b)((b, a) => f(a, b))

  /** get the head if the [[ConsList]] is not empty */
  final def headOption: Option[A] = this match {
    case CNil()     => Option.empty
    case Cons(h, _) => Some(h)
  }

  /** check if a [[ConsList]] is empty */
  final def isEmpty: Boolean = this match {
    case CNil()     => true
    case Cons(_, _) => false
  }

  /** get the last element if the [[ConsList]] is not empty */
  final def lastOption: Option[A] = {
    this match {
      case CNil() => None
      case Cons(head, tail) =>
        var last = head
        var l = tail
        while(true){
          l match {
            case Cons(h, t) =>
              last = h
              l = t
            case CNil() => return Some(last)
          }
        }
        Some(last)
    }
  }

  /** get the element at the index if it exists */
  final def lookup(index: Int): Option[A] = {
    var l = this
    var i = index
    while(true){
      l match {
        case Cons(h, t) =>
          if(i > 0){ i = i - 1; l = t }
          else if(i == 0) return Some(h)
          else return None
        case CNil() => return None
      }
    }
    None
  }

  final def map[@miniboxed B](f: A => B): ConsList[B] = {
    var acc = empty[B]
    var l = this
    while(true){
      l match {
        case Cons(h, t) =>
          acc = Cons(f(h), acc)
          l = t
        case CNil() =>
          var acc2 = empty[B]
          while(true){
          acc match {
            case Cons(h, t) =>
              acc2 = Cons(h, acc2)
              acc = t
            case CNil() => return acc2
          }
        }
      }
    }
    acc
  }

  /** add an element to the front */
  final def prepend(a: A): ConsList[A] =
    Cons(a, this)

  /** alias for prepend */
  final def ::(a: A): ConsList[A] =
    Cons(a, this)

  /** reverse a [[ConsList]] */
  final def reverse: ConsList[A] = {
    var acc = empty[A]
    var l = this
    while(true){
      l match {
        case Cons(h, t) => acc = Cons(h , acc); l = t
        case CNil() => return acc
      }
    }
    acc
  }

  /** compute the size of a [[ConsList]] */
  final def size: Int = {
    var acc = 0
    var l = this
    while(true){
      l match {
        case Cons(_, t) =>
          acc = acc + 1
          l = t
        case CNil() => return acc
      }
    }
    acc
  }

  /** get the tail if the [[ConsList]] is not empty */
  final def tailOption: Option[ConsList[A]] = this match {
    case CNil()     => Option.empty
    case Cons(_, t) => Some(t)
  }

  /** take the `n` first elements */
  final def take(n: Int): ConsList[A] = {
    var acc = empty[A]
    var l = this
    var m = n
    while(true){
      l match {
        case Cons(h, t) =>
          if(m > 0){ m = m - 1; l = t; acc = Cons(h, acc) }
          else return acc.reverse
        case CNil() => return acc.reverse
      }
    }
    acc
  }

  /** take elements as long as the predicate holds */
  final def takeWhile(p: A => Boolean): ConsList[A] = {
    var acc = empty[A]
    var l = this
    while(true){
      l match {
        case Cons(h, t) =>
          if(p(h)){ l = t; acc = Cons(h, acc) }
          else return acc.reverse
        case CNil() => return acc.reverse
      }
    }
    acc
  }

  /** transform a [[ConsList]] into a [[scala.List]] */
  final def toList: List[A] =
    foldLeft(ListBuffer.empty[A])(_ += _).toList

  /** attempt to get head and tail of a [[ConsList]] */
  final def uncons: Option[(A, ConsList[A])] = this match {
    case CNil()     => Option.empty
    case Cons(h, t) => Some((h,t))
  }

  /** widen the type of a [[ConsList]] */
  final def widen[B >: A]: ConsList[B] =
    this.asInstanceOf[ConsList[B]]

//  /** check if two [[ConsList]] are equal */
//  final def ===(other: ConsList[A])(implicit A: Eq[A]): Boolean = {
//    var as = this
//    var bs = other
//    while(true){
//      (as, bs) match {
//        case (CNil(), CNil()) => return true
//        case (Cons(x, xs), Cons(y, ys)) =>
//          if(A.eqv(x,y)){ as = xs; bs = ys }
//          else return false
//        case _ => return false
//      }
//    }
//    true
//  }

}

object ConsList extends ConsListInstances {
  final case class CNil[@miniboxed A]() extends ConsList[A]
  final case class Cons[@miniboxed A](head: A, tail: ConsList[A]) extends ConsList[A]

  private val nil: ConsList[Nothing] = CNil()

  /** create a [[ConsList]] with a single element */
  def singleton[@miniboxed A](a: A): ConsList[A] =
    Cons(a, empty)

  /** create an empty [[ConsList]] */
  def empty[@miniboxed A]: ConsList[A] =
    nil.asInstanceOf[ConsList[A]]

  /** create a [[ConsList]] from a varargs */
  def apply[@miniboxed A](as: A*): ConsList[A] =
    as.foldRight(empty[A])(Cons(_,_))

}

sealed abstract class ConsListInstances {
//  implicit def ilistEq[@miniboxed A: Eq]: Eq[ConsList[A]] = new Eq[ConsList[A]]{
//    def eqv(x: ConsList[A], y: ConsList[A]): Boolean =
//      x === y
//  }
//
//  implicit def ilistMonoid[@miniboxed A]: Monoid[ConsList[A]] = new Monoid[ConsList[A]] {
//    def empty: ConsList[A] =
//      ConsList.empty
//
//    def combine(x: ConsList[A], y: ConsList[A]): ConsList[A] =
//      x concat y
//  }
}
