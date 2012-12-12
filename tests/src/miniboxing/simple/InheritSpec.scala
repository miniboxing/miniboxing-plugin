package miniboxing.simple

import miniboxing.plugin.minispec
import scala.util.Random

// XXX: inheritance related tests do not work
class Base {
  val b = 0
}

trait SpecTrait[@minispec A, @minispec B] {
  def st1(t: A): B
  def st2: A
}


class Foo[@minispec T: Manifest](p: T) extends Base with SpecTrait[T, T] {
  def st1(t: T): T = t
  def st2 = p

  def toString1() = p.toString
  def toString2() = st2.toString
}

class Bar[@minispec T: Manifest](t: T) extends Foo[T](t: T) {
  override def toString1() = t.toString
}


class Top extends Bar[Int](2) {
  def method = {
    val x : Bar[_] = if (Random.nextInt == 0) new Bar[Double](2d) else new Top
    x
  }
}

