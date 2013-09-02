package miniboxing.tests.compile

import scala.annotation.tailrec

class Cplx[@miniboxed A](real: A, imag: A)(implicit f: Fractional[A]) {
  def isZero: Boolean = ???
  def abs: A = ???
  def %(that: Cplx[A]): Cplx[A] = ???
  def /~(that: Cplx[A]): Cplx[A] = ???
  def /%(that: Cplx[A]): Cplx[A] = ???
}

object Cplx {
  def one[T](implicit f: Fractional[T]): Cplx[T] = ???
}

trait CplxIsRing[@miniboxed A] {
  implicit def f: Fractional[A]
  def one: Cplx[A] = Cplx.one
}

trait Ord[@miniboxed A] {
  def lt(o1: A, o2: A): Boolean
}

trait Fractional[@miniboxed A] extends Ord[A] {
  def one: A
  def zero: A
}


trait CplxIsEuclideanRing[@miniboxed A] extends CplxIsRing[A] {
  def quot(a: Cplx[A], b: Cplx[A]) = a /~ b
  def mod(a: Cplx[A], b: Cplx[A]) = a % b
  def quotmod(a: Cplx[A], b: Cplx[A]) = a /% b
  def gcd(a: Cplx[A], b: Cplx[A]): Cplx[A] = {
    @tailrec def _gcd(a: Cplx[A], b: Cplx[A]): Cplx[A] = {
      if (f.lt(a.abs,f.one)) one
      else if (b.isZero) a
      else if (f.lt(b.abs,f.one)) one
      else _gcd(b, a % b)
    }
    _gcd(a, b)
  }
}
