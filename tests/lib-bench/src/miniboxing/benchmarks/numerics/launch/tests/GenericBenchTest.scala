package miniboxing.benchmarks.numerics.launch.tests

import miniboxing.benchmarks.numerics.generic.Matrix
import scala.util.Random

trait GenericBenchTest extends BaseTest {

  def testGeneric(matrix: Array[Array[Double]], rows: Int, cols: Int) = {
    val m = new Matrix(matrix, rows, cols)
    test(
      "generic",
      "scalar_addition",
      _ => (),
      m + 13.0,
      () => {}
    )
    test(
      "generic",
      "scalar_multiplication",
      _ => (),
      m * 9.0,
      () => {}
    )
    test(
      "generic",
      "matrix_addition",
      _ => (),
      m + m,
      () => {}
    )
    test(
      "generic",
      "matrix_multiplication",
      _ => (),
      m * m,
      () => {}
    )
  }
}
