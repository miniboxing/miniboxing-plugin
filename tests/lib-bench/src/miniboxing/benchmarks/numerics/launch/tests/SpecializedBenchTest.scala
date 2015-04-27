package miniboxing.benchmarks.numerics.launch.tests

import miniboxing.benchmarks.numerics.specialized.Matrix
import scala.util.Random

trait SpecializedBenchTest extends BaseTest {

  def testSpecialized(matrixD: Array[Array[Double]], matrixI: Array[Array[Int]], matrixL: Array[Array[Long]], matrixS: Array[Array[Short]], rows: Int, cols: Int, rowsForMul: Int, colsForMul: Int) = {
    val mD = new Matrix(matrixD, rows, cols)
    val mDMul = new Matrix(matrixD, rowsForMul, colsForMul)
    val mI = new Matrix(matrixI, rows, cols)
    val mL = new Matrix(matrixL, rows, cols)
    val mS = new Matrix(matrixS, rows, cols)
    test(
      "specialized",
      "scalar_addition",
      _ => {mS + Short.box(8); mI * 9; mL * 8; mD * 7.0},
      {mS + Short.box(5); mI * 6; mL * 5; mD * 4.0},
      () => {}
    )
    test(
      "specialized",
      "scalar_multiplication",
      _ => {mS + Short.box(8); mI * 9; mL * 8; mD * 7.0},
      {mS + Short.box(5); mI * 6; mL * 5; mD * 4.0},
      () => {}
    )
    test(
      "specialized",
      "matrix_addition",
      _ => (),
      mD + (2.9,mD),
      () => {}
    )
    test(
      "specialized",
      "matrix_multiplication",
      _ => (),
      mDMul * (2.9,mDMul),
      () => {}
    )
  }
}
