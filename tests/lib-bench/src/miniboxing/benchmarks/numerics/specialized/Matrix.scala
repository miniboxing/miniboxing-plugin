package miniboxing.benchmarks.numerics.specialized

import Predef.{any2stringadd => _, StringAdd => _, _}
import scala.reflect.ClassTag
import miniboxing.benchmarks.numerics.specialized.math.SpecializedNumeric

class MatrixException(s:String) extends Exception

/**
 * Matrix implementation in terms of a generic numeric type.
 */
class Matrix[@specialized A: ClassTag](val data: Array[Array[A]], val rows: Int, val cols: Int)(implicit numeric: SpecializedNumeric[A]) extends Serializable {
  import numeric._
  
  if (rows < 1) throw new MatrixException("illegal height")
  if (cols < 1) throw new MatrixException("illegal widht")

  def apply(i: Int, j: Int) = data(i)(j)
  def update(i: Int, j: Int, value: A) = data(i)(j) = value
  def createEmpty() = Matrix.empty[A](rows, cols)
  
  def map[@specialized B: ClassTag](arg: A, f:(A) => B)(implicit num: SpecializedNumeric[B]) = 
    new Matrix[B](data.map(t => t.map(s => f(s))), rows, cols)

  /* combine two matrices element-by-element */
  def combine(arg: A, rhs:Matrix[A], f:(A, A) => A) = {
    val result = createEmpty
    for (i <- 0 until rows; j <- 0 until cols) {
      result(i, j) = f(this(i, j), rhs(i, j))
    }
    result
  }

  /* add a scalar value to each element */
  def +(a:A)():Matrix[A] = map(a, _ + a)

  /* add two matrices */
  def +(arg: A, rhs:Matrix[A]):Matrix[A] = combine(arg, rhs, _ + _)

  /* multiply each element by a scalar value */
  def *(a:A):Matrix[A] = map(a, _ * a)
  
  /* multiply two matrices */
  def *(arg: A, rhs:Matrix[A]):Matrix[A] = {

    /* make sure this and rhs are compatible */
    if (this.rows != rhs.cols || this.cols != rhs.rows) {
      throw new MatrixException("dimensions do not match")
    }

    /* figure out the dimensions of the result matrix */
    val (rrows, rcols, n) = (this.rows, rhs.cols, this.cols)

    /* allocate the result matrix */
    val result = Matrix.empty[A](rows, rcols)

    /* loop over the cells in the result matrix */
    var i: Int = 0
    while (i < rrows) {
      var j: Int = 0
      while (j < rcols) {
        var sum: A = numeric.zero
        var k: Int = 0
        while (k < n) {
          sum = sum + this(i, k) * rhs(k, j)
          k = k + 1
        }
        result(i, j) = sum
        j = j + 1
      }
      i = i + 1
    }
    result
  }

  def toAscii() = "[" + data.map {
    _.foldLeft("")(_ + " " + _.toString)
  }.reduceLeft(_ + "\n" + _) + "]"
}

object Matrix {
  def empty[@specialized A: ClassTag](rows:Int, cols:Int)(implicit numeric: SpecializedNumeric[A]) = {
    new Matrix(Array.ofDim[A](rows, cols), rows, cols)
  }

  def apply[@specialized A: ClassTag](data:Array[Array[A]], rows:Int, cols:Int)(implicit numeric: SpecializedNumeric[A]) = {
    new Matrix(data, rows, cols)
  }
}
