package miniboxing.benchmarks.numerics.ideal

class MatrixException(s:String) extends Exception

/**
 * Matrix implementation in terms of Double.
 */
class DMatrix(val data:Array[Array[Double]], val rows:Int, val cols:Int) extends Serializable {
  
  if (rows < 1) throw new MatrixException("illegal height")
  if (cols < 1) throw new MatrixException("illegal widht")

  /* build an empty matrix with the same dimensions */
  def createEmpty() = DMatrix.empty(rows, cols)

  /* access the element at (y, x) */
  def apply(y:Int, x:Int) = data(y)(x)

  /* update the element at (y, x) to value */
  def update(y:Int, x:Int, value:Double) = data(y)(x) = value

  /* create a new Matrix by mapping each element through f */
  def map(f:Double => Double) = {
    new DMatrix(data.map(_.map(f).toArray).toArray, rows, cols)
  }

  /* combine two matrices element-by-element */
  def combine(rhs:DMatrix, f:(Double, Double) => Double) = {
    val result = createEmpty
    for (y <- 0 until rows; x <- 0 until cols) {
      result(y, x) = f(this(y, x), rhs(y, x))
    }
    result
  }

  /* add a scalar value to each element */
  def +(a:Double) = map(_ + a)

  /* add two matrices */
  def +(rhs:DMatrix) = combine(rhs, _ + _)

  /* multiply each element by a scalar value */
  def *(a:Double) = map(_ * a)
  
  /* multiply two matrices */
  def *(rhs:DMatrix) = {

    /* make sure this and rhs are compatible */
    if (this.rows != rhs.cols || this.cols != rhs.rows) {
      throw new MatrixException("dimensions do not match")
    }

    val (rrows, rcols, n) = (this.rows, rhs.cols, this.cols)

    val result = DMatrix.empty(rrows, rcols)

    for(y <- 0 until rrows; x <- 0 until rcols) {
      result(y, x) = (0 until n).foldLeft(0.0) {
        case (sum, i) => sum + (this(y, i) * rhs(i, x))
      }
    }

    result
  }

  def toAscii() = "[" + data.map {
    _.foldLeft("")(_ + " " + _.toString)
  }.reduceLeft(_ + "\n" + _) + "]\n"
}

object DMatrix {
  def empty(rows:Int, cols:Int) = {
    new DMatrix(Array.ofDim[Double](rows, cols), rows, cols)
  }

  def apply(data:Array[Array[Double]], rows:Int, cols:Int) = {
    new DMatrix(data, rows, cols)
  }
}
