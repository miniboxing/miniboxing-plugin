package miniboxing.benchmarks.rrbvector.launch.tests

import scala.util.Random
import miniboxing.benchmarks.rrbvector.miniboxed._

trait MiniboxedBenchTest extends BaseTest {

  def testMiniboxed() = {

    val rrbVectorBuilderInt = RRBVector.newBuilder[Int]
    val rrbVectorBuilderDouble = RRBVector.newBuilder[Double]
    for (i <- 0 to testSize) {
      rrbVectorBuilderInt += Random.nextInt()
      rrbVectorBuilderDouble += Random.nextDouble()
    }
    val rrbVectorInt = rrbVectorBuilderInt.result()
    val rrbVectorDouble = rrbVectorBuilderDouble.result()

    test(
      "miniboxed",
      "builder",
      _ => {},
      {
        val rrbVectorBuilder = RRBVector.newBuilder[Int]
        var i = 0
        while (i < testSize) {
          rrbVectorBuilder += i
          i += 1
        }
        rrbVectorBuilder.result()
      },
      () => {}
    )

    test(
      "miniboxed",
      "map",
      _ => {},
      {
        rrbVectorInt.map { x => x + 1 }
      },
      () => {}
    )

    test(
      "miniboxed",
      "fold",
      _ => {},
      {
        rrbVectorInt.fold(0)((r, c) => r + c)
      },
      () => {}
    )

    test(
      "miniboxed",
      "reverse",
      _ => {},
      {
        rrbVectorInt.reverse
      },
      () => {}
    )

    test(
      "miniboxed",
      "llsr",
      _ => {},
      {
       val llsr = new MbLLSRegression(rrbVectorDouble, rrbVectorDouble)
       llsr.calc_y(100.0)
      },
      () => {}
    )
  }
}
