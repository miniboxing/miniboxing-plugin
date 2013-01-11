package miniboxing.benchmarks.launch

import org.scalameter.api._
import miniboxing.runtime.MiniboxConstants._
import miniboxing.runtime.MiniboxConversions._
import miniboxing.benchmarks.hardcoded._

object HardcodedMiniboxingClassLoadFullBenchTest extends BenchTest {

  object TestList {
    def list_insert(): MBList[Int] = {
      var l: MBList[Int] = null
      var i = 0
      while (i < N) {
        l = new MBList_INT_FULL[Int](IntToMinibox(i), l)
        i += 1
      }
      l
    }

    def list_hashCode(list: MBList[Int]): Int = {
      list.hashCode_J
    }

    def list_find(l: MBList[Int]): Boolean = {
      var i = 0
      var b = true
      while (i < N) {
        b = b ^ l.contains_J(i.toLong)
        i += 10000
      }
      b
    }
  }

  object TestArray {
    def array_insert(): MBResizableArray[Int] = {
      val a: MBResizableArray[Int] = new MBResizableArray_INT_FULL[Int]()
      var i = 0
      while (i < N) {
        a.add_J(IntToMinibox(i))
        i += 1
      }
      a
    }

    def array_reverse(a: MBResizableArray[Int]): MBResizableArray[Int] = {
      a.reverse_J
      a
    }

    def array_find(a: MBResizableArray[Int]): Boolean = {
      var i = 0
      var b = true
      while (i < N) {
        b = b ^ a.contains_J(IntToMinibox(i)) // TODO: Does this cost much?
        i += 10000
      }
      b
    }
  }

  import TestArray._
  import TestList._

  System.gc

  println("TESTING HARDCODED MINIBOXED w/ SIMULATED CLASSLOADING FULL REPLACEMENT")
  val prefix = "miniboxed w/cl "

  var a: MBResizableArray[Int] = null
  var b: Boolean = true
  test(prefix + "array", "insert ", _ => (),                 a = array_insert(),   () => { assert(a.length == N); a = null })
  test(prefix + "array", "reverse", _ => a = array_insert(), a = array_reverse(a), () => { assert(a.length == N); a = null })
  test(prefix + "array", "find   ", _ => a = array_insert(), b = array_find(a),    () => { assert(b == true); a = null })

  var l: MBList[Int] = null
  var i: Int = 0
  test(prefix + "list", "insert  ", _ => (),                 l = list_insert(),    () => { assert(l.length == N); l = null })
  test(prefix + "list", "hashCode", _ => l = list_insert(),  i = list_hashCode(l), () => { assert(i != 0); l = null })
  test(prefix + "list", "find    ", _ => l = list_insert(),  b = list_find(l),     () => { assert(b == true); l = null })
}
