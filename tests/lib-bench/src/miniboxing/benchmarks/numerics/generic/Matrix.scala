package miniboxing.benchmarks.numerics.generic

import Predef.{any2stringadd => _, StringAdd => _, _}
import scala.reflect.ClassTag

class MatrixException(s:String) extends Exception

/**
 * Matrix implementation in terms of a generic numeric type.
 */
class Matrix[A: ClassTag](val data: Array[Array[A]], val rows: Int, val cols: Int)(implicit numeric: Numeric[A]) extends Serializable {
  import numeric._
  
  if (rows < 1) throw new MatrixException("illegal height")
  if (cols < 1) throw new MatrixException("illegal widht")

  def apply(i: Int, j: Int) = data(i)(j)
  def update(i: Int, j: Int, value: A) = data(i)(j) = value
  def createEmpty() = Matrix.empty[A](rows, cols)
  
  def map[B: ClassTag](f:(A) => B)(implicit num: Numeric[B]) = 
    new Matrix[B](data.map(t => t.map(s => f(s))), rows, cols)

  /* combine two matrices element-by-element */
  def combine(rhs:Matrix[A], f:(A, A) => A) = {
    val result = createEmpty
    for (i <- 0 until rows; j <- 0 until cols) {
      result(i, j) = f(this(i, j), rhs(i, j))
    }
    result
  }

  /* add a scalar value to each element */
  def +(a:A)():Matrix[A] = map(_ + a)

  /* add two matrices */
  def +(rhs:Matrix[A]):Matrix[A] = combine(rhs, _ + _)

  /* multiply each element by a scalar value */
  def *(a:A):Matrix[A] = map(_ * a)
  
  /* multiply two matrices */
  def *(rhs:Matrix[A]):Matrix[A] = {

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
  def empty[A: ClassTag](rows:Int, cols:Int)(implicit numeric: Numeric[A]) = {
    new Matrix(Array.ofDim[A](rows, cols), rows, cols)
  }

  def apply[A: ClassTag](data:Array[Array[A]], rows:Int, cols:Int)(implicit numeric: Numeric[A]) = {
    new Matrix(data, rows, cols)
  }
}
