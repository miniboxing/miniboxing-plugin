package miniboxing.benchmarks.numerics.launch.tests

import scala.util.Random
import miniboxing.benchmarks.numerics.miniboxed.hardcoded.Matrix

trait HardcodedMiniboxedBenchTest extends BaseTest {

  def testHardcodedMiniboxed(matrix: Array[Array[Double]], rows: Int, cols: Int) = {
    val m = new Matrix(matrix, rows, cols)
    test(
      "miniboxed_hardcoded",
      "scalar_addition",
      _ => (),
      m + 13.0,
      () => {}
    )
    test(
      "miniboxed_hardcoded",
      "scalar_multiplication",
      _ => (),
      m * 9.0,
      () => {}
    )
    test(
      "miniboxed_hardcoded",
      "matrix_addition",
      _ => (),
      m + m,
      () => {}
    )
    test(
      "miniboxed_hardcoded",
      "matrix_multiplication",
      _ => (),
      m * m,
      () => {}
    )
  }
}
