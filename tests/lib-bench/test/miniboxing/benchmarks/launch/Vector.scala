package miniboxing
package benchmarks
package launch

import org.scalameter._
import org.scalameter.api._


//object MiniboxedVectorBenchmark extends TweakedPerfomanceTest {
//
//  import simple.miniboxed._
//
//  // sanity check:
//  lazy val fun = new Function1[Double, Double] { def apply(d: Double) = d }
//  assert(fun.getClass().getInterfaces().map(_.getSimpleName()).contains("Function1_DD"), fun.getClass().getInterfaces())
//
//  implicit object Num_D extends Numeric[Double] {
//    def plus(x: Double, y: Double): Double = x + y
//    def zero: Double = 0.0
//  }
//
//   var vectx: List[Double] = _
//   var vecty: List[Double] = _
//
//  if (tests.contains(miniboxed)) {
//
//    report("miniboxed")
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
//            vectx = Vector[Double](0)
//            vecty = Vector[Double](0)
//            var i = 0
//            while (i < size) {
//              vectx = (i + rand) :: i + rand :: vectx
//              vecty = (func(i) + rand) :: func(i) + rand :: vecty
//              i += 1
//            }
//        } config (
//            testSettings: _*
//        ) in {
//          size =>
//
//          val vectxy = vectx.zip(vecty)
//
//          val sumx  = vectx.sum
//          val sumy  = vecty.sum
//
//          // function (x, y) => x * y
//          val fxy = new Function1[Tuple2[Double,Double], Double] {
//            def apply(t: Tuple2[Double, Double]): Double = t._1 * t._2
//          }
//          val sumxy = vectxy.map(fxy).sum
//
//          // function x => x * x
//          val fxx = new Function1[Double, Double] {
//            def apply(x: Double): Double = x * x
//          }
//          val squarex = vectx.map(fxx).sum
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
//object GenericVectorBenchmark extends TweakedPerfomanceTest {
//
//  import simple.generic._
//
//  // sanity check:
//  lazy val fun = new Function1[Int, Int] { def apply(i: Int) = i }
//  assert(fun.getClass().getInterfaces().map(_.getSimpleName()).contains("Function1"), fun.getClass().getInterfaces())
//
//  implicit object Num_D extends Numeric[Double] {
//    def plus(x: Double, y: Double): Double = x + y
//    def zero: Double = 0.0
//  }
//
//   var vectx: List[Double] = _
//   var vecty: List[Double] = _
//
//  if (tests.contains(generic)) {
//
//    report("generic")
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
//            vectx = Vector[Double](0)
//            vecty = Vector[Double](0)
//            var i = 0
//            while (i < size) {
//              vectx = vectx :+ (i + rand)
//              vecty = vecty :+ (func(i) + rand)
//              i += 1
//            }
//        } config (
//            testSettings: _*
//        ) in {
//          size =>
//
//          val vectxy = vectx.zip(vecty)
//
//          val sumx  = vectx.sum
//          val sumy  = vecty.sum
//
//          // function (x, y) => x * y
//          val fxy = new Function1[Tuple2[Double,Double], Double] {
//            def apply(t: Tuple2[Double, Double]): Double = t._1 * t._2
//          }
//          val sumxy = vectxy.map(fxy).sum
//
//          // function x => x * x
//          val fxx = new Function1[Double, Double] {
//            def apply(x: Double): Double = x * x
//          }
//          val squarex = vectx.map(fxx).sum
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
//object SpecializedVectorBenchmark extends TweakedPerfomanceTest {
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
//   var vectx: List[Double] = _
//   var vecty: List[Double] = _
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
//            vectx = Vector[Double](0)
//            vecty = Vector[Double](0)
//            var i = 0
//            while (i < size) {
//              vectx = vectx :+ (i + rand)
//              vecty = vecty :+ (func(i) + rand)
//              i += 1
//            }
//        } config (
//            testSettings: _*
//        ) in {
//          size =>
//
//          val vectxy = vectx.zip(vecty)
//
//          val sumx  = vectx.sum
//          val sumy  = vecty.sum
//
//          // function (x, y) => x * y
//          val fxy = new Function1[Tuple2[Double,Double], Double] {
//            def apply(t: Tuple2[Double, Double]): Double = t._1 * t._2
//          }
//          val sumxy = vectxy.map(fxy).sum
//
//          // function x => x * x
//          val fxx = new Function1[Double, Double] {
//            def apply(x: Double): Double = x * x
//          }
//          val squarex = vectx.map(fxx).sum
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

object LibraryVectorGenericBenchmark extends TweakedPerfomanceTest {

  // sanity check:
  lazy val fun = new Function1[Int, Int] { def apply(i: Int) = i }
  assert(fun.getClass().getInterfaces().map(_.getSimpleName()).exists(_.endsWith("Function1$mcII$sp")), fun.getClass().getInterfaces())

  var vectx: Vector[Double] = _
  var vecty: Vector[Double] = _

  if (tests.contains(library)) {

    report("library")
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
            vectx = Vector[Double](0)
            vecty = Vector[Double](0)
            var i = 0
            while (i < size) {
              vectx = vectx :+ (i + rand)
              vecty = vecty :+ (func(i) + rand)
              i += 1
            }
        } config (
            testSettings: _*
        ) in {
          size =>

          val vectxy = vectx.zip(vecty)

          val sumx  = vectx.sum
          val sumy  = vecty.sum

          val sumxy = vectxy.map(t => t._1 * t._2).sum
          val squarex = vectx.map(x => x * x).sum

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
//
//object LibraryScalaBlitzBenchmark extends TweakedPerfomanceTest {
//
//  // sanity check:
//  lazy val fun = new Function1[Int, Int] { def apply(i: Int) = i }
//  assert(fun.getClass().getSimpleName() == "Function1$mcII$sp")
//
//   var vectx: List[Double] = _
//   var vecty: List[Double] = _
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
//            vectx = Vector[Double](0)
//            vecty = Vector[Double](0)
//            var i = 0
//            while (i < size) {
//              vectx = vectx :+ (i + rand)
//              vecty = vecty :+ (func(i) + rand)
//              i += 1
//            }
//        } config (
//            testSettings: _*
//        ) in {
//          size =>
//          import scala.collection.optimizer._
//          optimize {
//            val vectxy = vectx.zip(vecty)
//            val sumx  = vectx.sum
//            //  [error] /mnt/data1/Work/Workspace/dev/miniboxing-plugin/tests/lib-bench/test/miniboxing/benchmarks/launch/SimpleLinkedList.scala:292: value sum is not a member of scala.collection.par.Par[List[Double]]
//            //  [error]           val sumx  = vectx.sum
//            //  [error]               ^
//            //  [error] one error found
//            val sumy  = vecty.sum
//
//            val sumxy = vectxy.map(t => t._1 * t._2).sum
//            val squarex = vectx.map(x => x * x).sum
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
