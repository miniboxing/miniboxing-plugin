package miniboxing.benchmarks.rrbvector.launch.tests

import scala.util.Random
import miniboxing.benchmarks.rrbvector.erased._

trait GenericBenchTest extends BaseTest {

  def testGeneric() = {

    val rrbVectorBuilderInt = RRBVector.newBuilder[Int]
    val rrbVectorBuilderDouble = RRBVector.newBuilder[Double]
    for (i <- 0 to testSize) {
      rrbVectorBuilderInt += Random.nextInt()
      rrbVectorBuilderDouble += Random.nextDouble()
    }
    val rrbVectorInt = rrbVectorBuilderInt.result()
    val rrbVectorDouble = rrbVectorBuilderDouble.result()

    test(
      "generic",
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
      "generic",
      "map",
      _ => {},
      {
        rrbVectorInt.map { x => x + 1 }
      },
      () => {}
    )

    test(
      "generic",
      "fold",
      _ => {},
      {
        rrbVectorInt.fold(0)((r, c) => r + c)
      },
      () => {}
    )

    test(
      "generic",
      "reverse",
      _ => {},
      {
        rrbVectorInt.reverse
      },
      () => {}
    )

    test(
      "generic",
      "llsr",
      _ => {},
      {
       val llsr = new LLSRegression(rrbVectorDouble, rrbVectorDouble)
       llsr.calc_y(100.0)
      },
      () => {}
    )
  }
}
