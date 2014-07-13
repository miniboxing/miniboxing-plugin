package miniboxing
package benchmarks
package launch

import org.scalameter.KeyValue
import org.scalameter.api._
import org.scalameter.CurveData
import org.scalameter.utils.Tree
import org.scalameter.execution.LocalExecutor
import org.scalameter.Executor.Measurer
import org.scalameter.Key

trait TestConfiguration {

  // Benchmarks to run
  object miniboxed
  object specialized
  object generic
  object library
  object scalablitz

  val sizes = {
    Gen.range("size")(from = 100000, upto = 1500000, hop = 100000)
  }
  val tests = List(miniboxed, specialized, generic) // library

  // Generated data parameters:
  def step = 5.0
  def zero = 3.0

  def testSettings =
    Seq[KeyValue](
      exec.benchRuns -> 10,
      exec.minWarmupRuns -> 10,
      exec.minWarmupRuns -> 20,
      exec.independentSamples -> 1,
      exec.outliers.suspectPercent -> 0,
      exec.jvmflags -> "-Xmx8g -Xms8g -Xss4m -XX:+UseParallelGC" // server config: -Xmx16g -Xms16g -Xss4m -XX:+CMSClassUnloadingEnabled -XX:ReservedCodeCacheSize=256m -XX:+TieredCompilation -XX:+UseNUMA
    )

  def ignoreRunsWithGC = false
}

trait TweakedPerfomanceTest extends PerformanceTest with TestConfiguration {

  @transient lazy val reporter = new LoggingReporter {

    override def report(result: CurveData, persistor: Persistor) {
      for (measurement <- result.measurements)
        print(f"  ${result.context.scope}%-60s: ${measurement.params}%-30s: ${measurement.value}% 10.5f\n")
    }

    override def report(result: Tree[CurveData], persistor: Persistor) = {
      true
    }
  }

  @transient lazy val executor = SeparateJvmsExecutor(
    Executor.Warmer.Default(),
    Aggregator.average,
    new Executor.Measurer.Default
  )

  def measurer: Measurer =
    if (ignoreRunsWithGC)
      new Measurer.IgnoringGC with Measurer.OutlierElimination with Measurer.RelativeNoise
    else
      new Measurer.Default with Measurer.OutlierElimination with Measurer.RelativeNoise

  def persistor = Persistor.None

  def report(bench: String) =
    println(s"Starting $bench benchmark. Lay back, it might take a few minutes to stabilize...")
}



object MiniboxedLinkedListBenchmark extends TweakedPerfomanceTest {

  import simple.miniboxed._

  // sanity check:
  lazy val fun = new Function1[Double, Double] { def apply(d: Double) = d }
  assert(fun.getClass().getInterfaces().map(_.getSimpleName()).contains("Function1_DD"), fun.getClass().getInterfaces())

  implicit object Num_D extends Numeric[Double] {
    def plus(x: Double, y: Double): Double = x + y
    def zero: Double = 0.0
  }

  var listx: List[Double] = Nil
  var listy: List[Double] = Nil

  if (tests.contains(miniboxed)) {

    report("miniboxed")
    performance of this.getClass.getSimpleName in {
      measure method "Least Squares Method with List[Double]" in {
        using(sizes) setUp {
          size =>
            System.gc()
            val random = new scala.util.Random(0)
            // Random between (-1.0, 1.0), mean = 0
            def rand = random.nextDouble - random.nextDouble
            // Function to approximate = 5x + 3
            val func = new Function1[Int, Double] {
              def apply(x: Int): Double = step*x + zero
            }
            listx = Nil
            listy = Nil
            var i = 0
            while (i < size) {
              listx = (i + rand) :: i + rand :: listx
              listy = (func(i) + rand) :: func(i) + rand :: listy
              i += 1
            }
        } config (
            testSettings: _*
        ) in {
          size =>

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
}



object MiniboxedLinkedListWithLibraryFunctionsBenchmark extends TweakedPerfomanceTest {

  import simple.miniboxed_lib_functions._

  // sanity check:
  lazy val fun = (d: Double) => { sys.error("x"); d}
  try {
    fun(3.0)
    assert(false, "function did not execute")
  } catch {
    case t: Throwable =>
      val specMethod = t.getStackTrace()(1).getMethodName()
      val mboxMethod = t.getStackTrace()(2).getMethodName()
      assert(specMethod == "apply$mcDD$sp", specMethod)
      assert(mboxMethod == "apply_DD", mboxMethod)
  }

  implicit object Num_D extends Numeric[Double] {
    def plus(x: Double, y: Double): Double = x + y
    def zero: Double = 0.0
  }

  var listx: List[Double] = Nil
  var listy: List[Double] = Nil

  if (tests.contains(miniboxed)) {

    report("miniboxed with bridges")
    performance of this.getClass.getSimpleName in {
      measure method "Least Squares Method with List[Double]" in {
        using(sizes) setUp {
          size =>
            ()
            System.gc()
            val random = new scala.util.Random(0)
            // Random between (-1.0, 1.0), mean = 0
            def rand = random.nextDouble - random.nextDouble
            // Function to approximate = 5x + 3
            val func = new Function1[Int, Double] {
              def apply(x: Int): Double = step*x + zero
            }
            listx = Nil
            listy = Nil
            var i = 0
            while (i < size) {
              listx = (i + rand) :: i + rand :: listx
              listy = (func(i) + rand) :: func(i) + rand :: listy
              i += 1
            }
        } config (
            testSettings: _*
        ) in {
          size =>
            ()
          val listxy = listx.zip(listy)

          val sumx  = listx.sum
          val sumy  = listy.sum

          // function (x, y) => x * y
          val sumxy = listxy.map((t: Tuple2[Double, Double]) => t._1 * t._2).sum

          // function x => x * x
          val squarex = listx.map((x: Double) => x * x).sum

          val m = (size*sumxy - sumx*sumy) / (size*squarex - sumx*sumx)
          val b = (sumy*squarex - sumx*sumxy) / (size*squarex - sumx*sumx)

          // was it a good approximation?
          assert(m - step < 0.1, "m exceeded 10% of error : " + m + " instead of " + step)
          assert(b - zero < 0.1, "b exceeded 10% of error : " + b + " instead of " + zero)
        }
      }
    }
  }
}



object GenericLinkedListBenchmark extends TweakedPerfomanceTest {

  import simple.generic._

  // sanity check:
  lazy val fun = new Function1[Int, Int] { def apply(i: Int) = i }
  assert(fun.getClass().getInterfaces().map(_.getSimpleName()).contains("Function1"), fun.getClass().getInterfaces())

  implicit object Num_D extends Numeric[Double] {
    def plus(x: Double, y: Double): Double = x + y
    def zero: Double = 0.0
  }

  var listx: List[Double] = Nil
  var listy: List[Double] = Nil

  if (tests.contains(generic)) {

    report("generic")
    performance of this.getClass.getSimpleName in {
      measure method "Least Squares Method with List[Double]" in {
        using(sizes) setUp {
          size =>
            System.gc()
            val random = new scala.util.Random(0)
            // Random between (-1.0, 1.0), mean = 0
            def rand = random.nextDouble - random.nextDouble
            // Function to approximate = 5x + 3
            val func = new Function1[Int, Double] {
              def apply(x: Int): Double = step*x + zero
            }
            listx = Nil
            listy = Nil
            var i = 0
            while (i < size) {
              listx = (i + rand) :: i + rand :: listx
              listy = (func(i) + rand) :: func(i) + rand :: listy
              i += 1
            }
        } config (
            testSettings: _*
        ) in {
          size =>

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
}

//
//
//object SpecializedLinkedListBenchmark extends TweakedPerfomanceTest {
//
//  import simple.specialized._
//
//  // sanity check:
//  lazy val fun = new Function1[Int, Int] { def apply(i: Int) = i }
//  assert(fun.getClass().getInterfaces().map(_.getSimpleName()).contains("Function1$mcII$sp"), fun.getClass().getInterfaces())
//
//  implicit object Num_D extends Numeric[Double] {
//    def plus(x: Double, y: Double): Double = x + y
//    def zero: Double = 0.0
//  }
//
//  var listx: List[Double] = Nil
//  var listy: List[Double] = Nil
//
//  if (tests.contains(specialized)) {
//
//    report("specialized")
//    performance of this.getClass.getSimpleName in {
//      measure method "Least Squares Method with List[Double]" in {
//        using(sizes) setUp {
//          size =>
//            System.gc()
//            val random = new scala.util.Random(0)
//            // Random between (-1.0, 1.0), mean = 0
//            def rand = random.nextDouble - random.nextDouble
//            // Function to approximate = 5x + 3
//            val func = new Function1[Int, Double] {
//              def apply(x: Int): Double = step*x + zero
//            }
//            listx = Nil
//            listy = Nil
//            var i = 0
//            while (i < size) {
//              listx = (i + rand) :: i + rand :: listx
//              listy = (func(i) + rand) :: func(i) + rand :: listy
//              i += 1
//            }
//        } config (
//            testSettings: _*
//        ) in {
//          size =>
//
//          val listxy = listx.zip(listy)
//
//          val sumx  = listx.sum
//          val sumy  = listy.sum
//
//          // function (x, y) => x * y
//          val fxy = new Function1[Tuple2[Double,Double], Double] {
//            def apply(t: Tuple2[Double, Double]): Double = t._1 * t._2
//          }
//          val sumxy = listxy.map(fxy).sum
//
//          // function x => x * x
//          val fxx = new Function1[Double, Double] {
//            def apply(x: Double): Double = x * x
//          }
//          val squarex = listx.map(fxx).sum
//
//          val m = (size*sumxy - sumx*sumy) / (size*squarex - sumx*sumx)
//          val b = (sumy*squarex - sumx*sumxy) / (size*squarex - sumx*sumx)
//
//          // was it a good approximation?
//          assert(m - step < 0.1, "m exceeded 10% of error : " + m + " instead of " + step)
//          assert(b - zero < 0.1, "b exceeded 10% of error : " + b + " instead of " + zero)
//        }
//      }
//    }
//  }
//}
//
//
//
//object LibraryGenericBenchmark extends TweakedPerfomanceTest {
//
//  // sanity check:
//  lazy val fun = new Function1[Int, Int] { def apply(i: Int) = i }
//  assert(fun.getClass().getInterfaces().map(_.getSimpleName()).exists(_.endsWith("Function1$mcII$sp")), fun.getClass().getInterfaces())
//
//  var listx: List[Double] = Nil
//  var listy: List[Double] = Nil
//
//  if (tests.contains(library)) {
//
//    report("library")
//    performance of this.getClass.getSimpleName in {
//      measure method "Least Squares Method with List[Double]" in {
//        using(sizes) setUp {
//          size =>
//            System.gc()
//            val random = new scala.util.Random(0)
//            // Random between (-1.0, 1.0), mean = 0
//            def rand = random.nextDouble - random.nextDouble
//            // Function to approximate = 5x + 3
//            val func = new Function1[Int, Double] {
//              def apply(x: Int): Double = step*x + zero
//            }
//            listx = Nil
//            listy = Nil
//            var i = 0
//            while (i < size) {
//              listx = (i + rand) :: i + rand :: listx
//              listy = (func(i) + rand) :: func(i) + rand :: listy
//              i += 1
//            }
//        } config (
//            testSettings: _*
//        ) in {
//          size =>
//
//          val listxy = listx.zip(listy)
//
//          val sumx  = listx.sum
//          val sumy  = listy.sum
//
//          val sumxy = listxy.map(t => t._1 * t._2).sum
//          val squarex = listx.map(x => x * x).sum
//
//          val m = (size*sumxy - sumx*sumy) / (size*squarex - sumx*sumx)
//          val b = (sumy*squarex - sumx*sumxy) / (size*squarex - sumx*sumx)
//
//          // was it a good approximation?
//          assert(m - step < 0.1, "m exceeded 10% of error : " + m + " instead of " + step)
//          assert(b - zero < 0.1, "b exceeded 10% of error : " + b + " instead of " + zero)
//        }
//      }
//    }
//  }
//}



//object LibraryScalaBlitzBenchmark extends TweakedPerfomanceTest {
//
//  // sanity check:
//  lazy val fun = new Function1[Int, Int] { def apply(i: Int) = i }
//  assert(fun.getClass().getSimpleName() == "Function1$mcII$sp")
//
//  var listx: List[Double] = Nil
//  var listy: List[Double] = Nil
//
//  if (tests.contains(scalablitz)) {
//
//    report("library scalablitz")
//    performance of this.getClass.getSimpleName in {
//      measure method "Least Squares Method with List[Double]" in {
//        using(sizes) setUp {
//          size =>
//            System.gc()
//            val random = new scala.util.Random(0)
//            // Random between (-1.0, 1.0), mean = 0
//            def rand = random.nextDouble - random.nextDouble
//            // Function to approximate = 5x + 3
//            val func = new Function1[Int, Double] {
//              def apply(x: Int): Double = step*x + zero
//            }
//            listx = Nil
//            listy = Nil
//            var i = 0
//            while (i < size) {
//              listx = (i + rand) :: i + rand :: listx
//              listy = (func(i) + rand) :: func(i) + rand :: listy
//              i += 1
//            }
//        } config (
//            testSettings: _*
//        ) in {
//          size =>
//          import scala.collection.optimizer._
//          optimize {
//            val listxy = listx.zip(listy)
//            val sumx  = listx.sum
//            //  [error] /mnt/data1/Work/Workspace/dev/miniboxing-plugin/tests/lib-bench/test/miniboxing/benchmarks/launch/SimpleLinkedList.scala:292: value sum is not a member of scala.collection.par.Par[List[Double]]
//            //  [error]           val sumx  = listx.sum
//            //  [error]               ^
//            //  [error] one error found
//            val sumy  = listy.sum
//
//            val sumxy = listxy.map(t => t._1 * t._2).sum
//            val squarex = listx.map(x => x * x).sum
//
//            val m = (size*sumxy - sumx*sumy) / (size*squarex - sumx*sumx)
//            val b = (sumy*squarex - sumx*sumxy) / (size*squarex - sumx*sumx)
//          }
//
//          // was it a good approximation?
//          assert(m - step < 0.1, "m exceeded 10% of error : " + m + " instead of " + step)
//          assert(b - zero < 0.1, "b exceeded 10% of error : " + b + " instead of " + zero)
//        }
//      }
//    }
//  }
//}
