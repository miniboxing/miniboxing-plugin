package miniboxing.benchmarks.numerics.launch.tests

import scala.util.Random
import miniboxing.benchmarks.numerics.miniboxed.hardcoded.Matrix

trait HardcodedMiniboxedBenchTest extends BaseTest {

  def testHardcodedMiniboxed(matrixD: Array[Array[Double]], matrixI: Array[Array[Int]], matrixL: Array[Array[Long]], matrixS: Array[Array[Short]], rows: Int, cols: Int, rowsForMul: Int, colsForMul: Int) = {
    val mD = new Matrix(matrixD, rows, cols)
    val mDMul = new Matrix(matrixD, rowsForMul, colsForMul)
    val mI = new Matrix(matrixI, rows, cols)
    val mL = new Matrix(matrixL, rows, cols)
    val mS = new Matrix(matrixS, rows, cols)
    test(
      "miniboxed_hardcoded",
      "scalar_addition",
      _ => {mS + Short.box(12); mI + 13; mL + 14; mD + 15.0},
      {mS + Short.box(15); mI + 16; mL + 17; mD + 18.0},
      () => {}
    )
    test(
      "miniboxed_hardcoded",
      "scalar_multiplication",
      _ => {mS + Short.box(8); mI * 9; mL * 8; mD * 7.0},
      {mS + Short.box(5); mI * 6; mL * 5; mD * 4.0},
      () => {}
    )
    test(
      "miniboxed_hardcoded",
      "matrix_addition",
      _ => (),
      mD + mD,
      () => {}
    )
    test(
      "miniboxed_hardcoded",
      "matrix_multiplication",
      _ => (),
      mDMul * mDMul,
      () => {}
    )
  }
}
