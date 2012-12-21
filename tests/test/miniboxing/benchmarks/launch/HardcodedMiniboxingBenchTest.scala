package miniboxing.benchmarks.launch

import org.scalameter.api._
import miniboxing.runtime.MiniboxConstants._
import miniboxing.runtime.MiniboxConversions._
import miniboxing.benchmarks.hardcoded._

object HardcodedMiniboxingBenchTest extends BenchTest {

  object TestList {
    def list_insert(): MBList[Int] = {
      var l: MBList[Int] = null
      var i = 0
      while (i < N) {
        l = new MBList_J[Int](IntToMinibox(i), l, INT)
        i += 1
      }
      l
    }

    def list_hashCode(list: MBList[Int]): Int = {
      list.hashCode_J
    }

    def list_find(l: MBList[Int]) = {
      var i = 0
      while (i < N) {
        l contains_J(IntToMinibox(i))
        i += 10000
      }
    }
  }

  object TestArray {
    def array_insert(): MBResizableArray[Int] = {
      val a: MBResizableArray[Int] = new MBResizableArray_J[Int](INT)
      var i = 0
      while (i < N) {
        a.add_J(IntToMinibox(i))
        i += 1
      }
      a
    }

    def array_reverse(a: MBResizableArray[Int]) = {
      a.reverse_J
    }

    def array_find(a: MBResizableArray[Int]) = {
      var i = 0
      while (i < N) {
        a.contains_J(IntToMinibox(i))
        i += 10000
      }
    }
  }

  import TestArray._
  import TestList._

  System.gc

  println("TESTING HARDCODED MINIBOXED")

  var a: MBResizableArray[Int] = null
  test("miniboxed array", "insert ", _ => (),                 a = array_insert(), a = null)
  test("miniboxed array", "reverse", _ => a = array_insert(), array_reverse(a),   a = null)
  test("miniboxed array", "find   ", _ => a = array_insert(), array_find(a),      a = null)

  var l: MBList[Int] = null
  test("miniboxed list", "insert  ", _ => (),                 l = list_insert(),  l = null)
  test("miniboxed list", "hashCode", _ => l = list_insert(),  list_hashCode(l),   l = null)
  test("miniboxed list", "find    ", _ => l = list_insert(),  list_find(l),       l = null)
}
