package miniboxing.benchmarks.rrbvector.launch.tests

import scala.util.Random
import miniboxing.benchmarks.rrbvector.erased._

trait GenericBenchTest extends BaseTest {

  def testGeneric() = {

    val rrbVectorBuilder = RRBVector.newBuilder[Int]
    for (i <- 0 to testSize) {
      rrbVectorBuilder += Random.nextInt()
    }
    val rrbVector = rrbVectorBuilder.result()

    val rrbVectorBuilderRev = RRBVector.newBuilder[Int]
    for (i <- 0 to testSize) {
      rrbVectorBuilderRev += Random.nextInt()
    }
    val rrbVectorRev = rrbVectorBuilderRev.result()

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
        rrbVector.map { x => x + 1 }
      },
      () => {}
    )

    test(
      "generic",
      "fold",
      _ => {},
      {
        rrbVector.fold(0)((r, c) => r + c)
      },
      () => {}
    )

    test(
      "generic",
      "reverse",
      _ => {},
      {
        rrbVectorRev.reverse
      },
      () => {}
    )
  }
}
