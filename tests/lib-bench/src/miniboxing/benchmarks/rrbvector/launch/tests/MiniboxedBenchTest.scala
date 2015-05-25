package miniboxing.benchmarks.rrbvector.launch.tests

import scala.util.Random
import scala.collection.immutable.mbrrbvector._


trait MiniboxedBenchTest extends BaseTest {

  def testMiniboxed(len: Int, lenReverse: Int) = {
    val rrbVectorBuilder = RRBVector.newBuilder[Int]
    for (i <- 0 to len) {
      rrbVectorBuilder += Random.nextInt()
    }
    val rrbVector = rrbVectorBuilder.result()
    
    val rrbVectorBuilderRev = RRBVector.newBuilder[Int]
    for (i <- 0 to lenReverse) {
      rrbVectorBuilderRev += Random.nextInt()
    }
    val rrbVectorRev = rrbVectorBuilderRev.result()
    
    test(
      "miniboxed",
      "builder",
      _ => {},
      {
        val rrbVectorBuilder = RRBVector.newBuilder[Int]
        for (i <- 0 to len) {
          rrbVectorBuilder += Random.nextInt()
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
        rrbVector.map { x => x + 1 }
      },
      () => {}
    )
    
    test(
      "miniboxed",
      "fold",
      _ => {},
      {
        rrbVector.fold(0)((r, c) => r + c)
      },
      () => {}
    )
    
    test(
      "miniboxed",
      "reverse",
      _ => {},
      {
        rrbVectorRev.reverse
      },
      () => {}
    )
  }
}
