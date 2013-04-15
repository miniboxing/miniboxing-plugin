package miniboxing.library.bench

import tests._
import scalameter._

object BenchmarkingTest extends ScalameterBenchTest
                           with ScalaLibraryBenchTest
                           with MiniboxedHCBenchTest
                           with Serializable{

  lazy val testSizes = {
    List(100000)
  }

  // not used now:
  def megamorphicTestSize = 200000

  // run the tests:
  testScalaLibrary()
  testMiniboxedHCLibrary()
}


