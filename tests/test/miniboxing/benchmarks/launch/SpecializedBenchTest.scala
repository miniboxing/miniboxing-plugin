package miniboxing.benchmarks.launch

import org.scalameter.api._
import miniboxing.benchmarks.specialized._

object SpecializedBenchTest extends BenchTest {

  object TestList {
    def list_insert(): List[Int] = {
      var l: List[Int] = null
      var i = 0
      while (i < N) {
        l = new List[Int](i, l)
        i += 1
      }
      l
    }

    def list_hashCode(list: List[Int]): Int = {
      list.hashCode
    }

    def list_find(l: List[Int]) = {
      var i = 0
      while (i < N) {
        l contains(i)
        i += 10000
      }
    }
  }

  object TestArray {
    def array_insert(): ResizableArray[Int] = {
      val a: ResizableArray[Int] = new ResizableArray[Int]()
      var i = 0
      while (i < N) {
        a.add(i)
        i += 1
      }
      a
    }

    def array_reverse(a: ResizableArray[Int]) = {
      a.reverse
    }

    def array_find(a: ResizableArray[Int]) = {
      var i = 0
      while (i < N) {
        a.contains(i)
        i += 10000
      }
    }
  }

  import TestArray._
  import TestList._

  System.gc

  println("TESTING SPECIALIZED")

  var a: ResizableArray[Int] = null
  test("specialized array", "insert ", _ => (),                 a = array_insert(), a = null)
  test("specialized array", "reverse", _ => a = array_insert(), array_reverse(a),   a = null)
  test("specialized array", "find   ", _ => a = array_insert(), array_find(a),      a = null)

  var l: List[Int] = null
  test("specialized list", "insert  ", _ => (),                 l = list_insert(),  l = null)
  test("specialized list", "hashCode", _ => l = list_insert(),  list_hashCode(l),   l = null)
  test("specialized list", "find    ", _ => l = list_insert(),  list_find(l),       l = null)

}
