package miniboxing.tests.compile
import miniboxing.plugin.minispec

// https://github.com/miniboxing/miniboxing-plugin/issues/27

trait Field2[@minispec A] {
  def zero: A
  def plus(o1: A, o2: A): A
  def times(o1: A, o2: A): A
}

trait ArrayInnerProductSpace2[@minispec A] {
  def scalar: Field2[A]
  private[this] var z: A = _

  def dot(v: Array[A], w: Array[A], zero: A): A = {
    z = scalar.zero
    var i = 0
    while (i < v.length && i < w.length) {
      z = scalar.plus(z, scalar.times(v(i), w(i)))
      i += 1
    }
    z
  }
}
