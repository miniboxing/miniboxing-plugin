package miniboxing.benchmarks.rrbvector.miniboxed

import miniboxing.runtime.math.MiniboxedFractional

// Learn Least Square Regression using RRBVector
class MbLLSRegression[@miniboxed A](val x: RRBVector[A], val y: RRBVector[A])(implicit numeric: MiniboxedFractional[A]) {
  def sum_x: A = x.sum
  def sum_y: A = y.sum
  def sum_xy: A = {
    var sum = numeric.zero
    var i = 0
    while (i < x.length) {
      sum = numeric.plus(sum, numeric.times(x(i), y(i)))
      i = i + 1
    }
    sum
  }
  def sum_xx: A = {
    var sum = numeric.zero
    var i = 0
    while (i < x.length) {
      sum = numeric.plus(sum, numeric.times(x(i), x(i)))
      i = i + 1
    }
    sum
  }

  def b: A = {
    val a = numeric.times(numeric.fromInt(x.length), sum_xy)
    val b = numeric.times(sum_x, sum_y)
    val c = numeric.times(sum_x, sum_x)
    val d = numeric.times(numeric.fromInt(x.length), sum_xx)

    val e = numeric.minus(a, b)
    val f = numeric.minus(d, c)

    numeric.div(e, f)
  }

  def a: A = {
    val m = numeric.times(b, sum_x)
    numeric.div(numeric.minus(sum_y, m), numeric.fromInt(x.length))
  }

  def calc_y(for_x: A): A = {
    numeric.plus(a, numeric.times(b, for_x))
  }
}