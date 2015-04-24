package miniboxing.benchmarks.numerics.miniboxed.hardcoded

import Predef.{any2stringadd => _, StringAdd => _, _}
import miniboxing.runtime.math.MiniboxedNumeric
import scala.reflect.ClassTag

class MatrixException(s:String) extends Exception

/**
 * Matrix implementation in terms of a generic numeric type.
 */
class Matrix[@miniboxed A: ClassTag](val data: Array[Array[A]], val rows: Int, val cols: Int)(implicit numeric: MiniboxedNumeric[A]) extends Serializable {
  import numeric._
    
  if (rows < 1) throw new MatrixException("illegal height")
  if (cols < 1) throw new MatrixException("illegal widht")
  
  def apply(i: Int, j: Int) = data(i)(j)
  def update(i: Int, j: Int, value: A) = data(i)(j) = value
  def createEmpty() = Matrix.empty[A](rows, cols)
  
  def map[@miniboxed B: ClassTag](f:(A) => B)(implicit num: MiniboxedNumeric[B]) = 
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
  def +(a:A):Matrix[A] = map(numeric.plus(_, a))

  /* add two matrices */
  def +(rhs:Matrix[A]):Matrix[A] = combine(rhs, numeric.plus(_, _))

  /* multiply each element by a scalar value */
  def *(a:A):Matrix[A] = map(numeric.times(_, a))
  
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
    for(y <- 0 until rrows; x <- 0 until rcols) {
      /* for each pair of values in this-row/rhs-column, multiply them
       * and then sum to get the result value for this cell. */
      result(y, x) = (0 until n).foldLeft(numeric.zero) {
        case (sum, i) => numeric.plus(sum, numeric.times(this(y, i), rhs(i, x)))
      }
    }
    result
  }

  def toAscii() = "[" + data.map {
    _.foldLeft("")(_ + " " + _.toString)
  }.reduceLeft(_ + "\n" + _) + "]"
}

object Matrix {
  def empty[@miniboxed A: ClassTag](rows:Int, cols:Int)(implicit numeric: MiniboxedNumeric[A]) = {
    new Matrix(Array.ofDim[A](rows, cols), rows, cols)
  }

  def apply[@miniboxed A: ClassTag](data:Array[Array[A]], rows:Int, cols:Int)(implicit numeric: MiniboxedNumeric[A]) = {
    new Matrix(data, rows, cols)
  }
}
