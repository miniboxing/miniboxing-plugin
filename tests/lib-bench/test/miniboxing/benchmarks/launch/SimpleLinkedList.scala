package miniboxing
package benchmarks
package launch

import org.scalameter.CurveData
import org.scalameter.api._
import org.scalameter.utils.Tree
import org.scalameter.execution.LocalExecutor

class TweakedPerfomanceTest extends PerformanceTest {

  @transient lazy val reporter = new LoggingReporter {

    override def report(result: CurveData, persistor: Persistor) {
      for (measurement <- result.measurements)
        print(f"  ${result.context.scope}%-60s: ${measurement.params}%-30s: ${measurement.value}% 10.5f\n")
    }

    override def report(result: Tree[CurveData], persistor: Persistor) = {
      true
    }
  }

  @transient lazy val executor = LocalExecutor.apply( //SeparateJvmsExecutor(
    Executor.Warmer.Default(),
    Aggregator.complete(Aggregator.average),
    new Executor.Measurer.Default
  )

  def persistor = Persistor.None

  val sizes = Gen.range("size")(100000, 1500000, 100000)

  def report(bench: String) =
    println(s"Starting $bench benchmarks. Lay back, it might take a few minutes to stabilize...")
}

object MiniboxedBenchmark extends TweakedPerfomanceTest {

  import simple.miniboxed._

  // sanity check:
  lazy val fun = new Function1[Int, Int] { def apply(i: Int) = i }
  assert(fun.getClass().getInterfaces().map(_.getSimpleName()).contains("Function1_JJ"), fun.getClass().getInterfaces())

  implicit object Num_D extends Numeric[Double] {
    def plus(x: Double, y: Double): Double = x + y

    def zero: Double = 0.0
  }

  val random = new scala.util.Random(0)

  // Random between (-1.0, 1.0), mean = 0
  def rand = random.nextDouble - random.nextDouble

  // Function to approximate = 5x + 3
  val step = 5.0
  val zero = 3.0
  val func = new Function1[Int, Double] {
    def apply(x: Int): Double = step*x + zero
  }

  report("miniboxed")

  // Method approximates func with the Least Squares Method
  //   approximation function = m*x + b
  performance of this.getClass.getSimpleName in {
    measure method "Least Squares Method with List[Double]" in {
      using(sizes) in {
        size =>
        // generates random points from original function
        var listx: List[Double] = Nil
        var listy: List[Double] = Nil
        var i = 0
        while (i < size) {
          listx = (i + rand) :: i + rand :: listx
          listy = (func(i) + rand) :: func(i) + rand :: listy
          i += 1
        }

        val listxy = listx.zip(listy)

        val sumx  = listx.sum
        val sumy  = listy.sum

        // function (x, y) => x * y
        val fxy = new Function1[Tuple2[Double,Double], Double] {
          def apply(t: Tuple2[Double, Double]): Double = t._1 * t._2
        }
        val sumxy = listxy.map(fxy).sum

        // function x => x * x
        val fxx = new Function1[Double, Double] {
          def apply(x: Double): Double = x * x
        }
        val squarex = listx.map(fxx).sum

        val m = (size*sumxy - sumx*sumy) / (size*squarex - sumx*sumx)
        val b = (sumy*squarex - sumx*sumxy) / (size*squarex - sumx*sumx)

        // was it a good approximation?
        assert(m - step < 0.1, "m exceeded 10% of error : " + m + " instead of " + step)
        assert(b - zero < 0.1, "b exceeded 10% of error : " + b + " instead of " + zero)
      }
    }
  }
}

object GenericBenchmark extends TweakedPerfomanceTest {

  import simple.generic._

  // sanity check:
  lazy val fun = new Function1[Int, Int] { def apply(i: Int) = i }
  assert(fun.getClass().getInterfaces().map(_.getSimpleName()).contains("Function1"), fun.getClass().getInterfaces())

  implicit object Num_D extends Numeric[Double] {
    def plus(x: Double, y: Double): Double = x + y

    def zero: Double = 0.0
  }

  val random = new scala.util.Random(0)

  // Random between (-1.0, 1.0), mean = 0
  def rand = random.nextDouble - random.nextDouble

  // Function to approximate = 5x + 3
  val step = 5.0
  val zero = 3.0
  val func = new Function1[Int, Double] {
    def apply(x: Int): Double = step*x + zero
  }

  report("generic")

  // Method approximates func with the Least Squares Method
  //   approximation function = m*x + b
  performance of this.getClass.getSimpleName in {
    measure method "Least Squares Method with List[Double]" in {
      using(sizes) in {
        size =>
        // generates random points from original function
        var listx: List[Double] = Nil
        var listy: List[Double] = Nil
        var i = 0
        while (i < size) {
          listx = (i + rand) :: i + rand :: listx
          listy = (func(i) + rand) :: func(i) + rand :: listy
          i += 1
        }

        val listxy = listx.zip(listy)

        val sumx  = listx.sum
        val sumy  = listy.sum

        // function (x, y) => x * y
        val fxy = new Function1[Tuple2[Double,Double], Double] {
          def apply(t: Tuple2[Double, Double]): Double = t._1 * t._2
        }
        val sumxy = listxy.map(fxy).sum

        // function x => x * x
        val fxx = new Function1[Double, Double] {
          def apply(x: Double): Double = x * x
        }
        val squarex = listx.map(fxx).sum

        val m = (size*sumxy - sumx*sumy) / (size*squarex - sumx*sumx)
        val b = (sumy*squarex - sumx*sumxy) / (size*squarex - sumx*sumx)

        // was it a good approximation?
        assert(m - step < 0.1, "m exceeded 10% of error : " + m + " instead of " + step)
        assert(b - zero < 0.1, "b exceeded 10% of error : " + b + " instead of " + zero)
      }
    }
  }
}
