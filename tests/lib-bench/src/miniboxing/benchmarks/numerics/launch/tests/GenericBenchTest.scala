package miniboxing.benchmarks.numerics.launch.tests

import miniboxing.benchmarks.numerics.generic.Matrix
import scala.util.Random

trait GenericBenchTest extends BaseTest {

  def testGeneric(matrixD: Array[Array[Double]], matrixI: Array[Array[Int]], matrixL: Array[Array[Long]], matrixS: Array[Array[Short]], rows: Int, cols: Int, rowsForMul: Int, colsForMul: Int) = {
    val mD = new Matrix(matrixD, rows, cols)
    val mDMul = new Matrix(matrixD, rowsForMul, colsForMul)
    val mI = new Matrix(matrixI, rows, cols)
    val mL = new Matrix(matrixL, rows, cols)
    val mS = new Matrix(matrixS, rows, cols)
    test(
      "generic",
      "scalar_addition",
      _ => {mS + Short.box(8); mI * 9; mL * 8; mD * 7.0},
      {mS + Short.box(5); mI * 6; mL * 5; mD * 4.0},
      () => {}
    )
    test(
      "generic",
      "scalar_multiplication",
      _ => {mS + Short.box(8); mI * 9; mL * 8; mD * 7.0},
      {mS + Short.box(5); mI * 6; mL * 5; mD * 4.0},
      () => {}
    )
    test(
      "generic",
      "matrix_addition",
      _ => (),
      mD + mD,
      () => {}
    )
    test(
      "generic",
      "matrix_multiplication",
      _ => (),
      mDMul * mDMul,
      () => {}
    )
  }
}
