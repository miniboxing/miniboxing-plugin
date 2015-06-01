package miniboxing.benchmarks.rrbvector.erased

import scala.language.postfixOps

// Learn Least Square Regression using RRBVector
class LLSRegression(val x: RRBVector[Double], val y: RRBVector[Double]) {
  def sum_x: Double = x.sum
  def sum_y: Double = y.sum
  def sum_xy: Double = x zip y map { case (x, y) => x * y } sum
  def sum_xx: Double = x zip x map { case (x, y) => x * y } sum

  def b: Double = {
    val a = x.length * sum_xy
    val b = sum_x * sum_y
    val c = sum_x * sum_x
    val d = x.length * sum_xx

    val e = a - b
    val f = d - c

    e / f
  }

  def a: Double = (sum_y - b * sum_x) / x.length

  def calc_y(for_x: Double): Double = {
    a + b * for_x
  }
}