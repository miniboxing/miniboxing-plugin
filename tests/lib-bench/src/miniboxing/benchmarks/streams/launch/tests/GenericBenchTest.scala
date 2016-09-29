package miniboxing.benchmarks.streams.launch.tests

import miniboxing.benchmarks.streams.erased._

trait GenericBenchTest extends BaseTest {

  def testGeneric() = {

    var N: Int = testSize
    var v: Array[Long] = Array.tabulate(N)(i => i.toLong % 1000)
    var vHi: Array[Long] = Array.tabulate(1000000)(_.toLong)
    var vLo: Array[Long] = Array.tabulate(10)(_.toLong)

    test(
      "generic",
      "Sum",
      _ => {},
      {
        val sum: Long = Stream(v)
          .sum
        sum
      },
      () => {})

    test(
      "generic",
      "SumOfSquares",
      _ => {},
      {
        val sum: Long = Stream(v)
          .map(d => d * d)
          .sum
        sum
      },
      () => {})

    test(
      "generic",
      "SumOfSqEven",
      _ => {},
      {
        val res: Long = Stream(v)
          .filter(x => x % 2 == 0)
          .map(x => x * x)
          .sum
        res
      },
      () => {})

    test(
      "generic",
      "Cart",
      _ => {},
      {
        val sum: Long = Stream(vHi)
          .flatMap(d => Stream(vLo).map(dp => dp * d))
          .sum
        sum
      },
      () => {})
  }
}
