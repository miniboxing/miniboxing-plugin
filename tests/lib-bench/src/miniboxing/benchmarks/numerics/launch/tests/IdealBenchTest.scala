package miniboxing.benchmarks.numerics.launch.tests

import miniboxing.benchmarks.numerics.ideal.DMatrix
import scala.util.Random

trait IdealBenchTest extends BaseTest {

  def testIdeal(matrix: Array[Array[Double]], rows: Int, cols: Int, rowsForMul: Int, colsForMul: Int) = {
    val m = new DMatrix(matrix, rows, cols)
    val mMul = new DMatrix(matrix, rowsForMul, colsForMul)
    test(
      "ideal",
      "scalar_addition",
      _ => (),
      m + 13.0,
      () => {}
    )
    test(
      "ideal",
      "scalar_multiplication",
      _ => (),
      m * 9.0,
      () => {}
    )
    test(
      "ideal",
      "matrix_addition",
      _ => (),
      m + m,
      () => {}
    )
    test(
      "ideal",
      "matrix_multiplication",
      _ => (),
      mMul * mMul,
      () => {}
    )
  }
}
