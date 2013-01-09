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

    def list_find(l: List[Int]): Boolean = {
      var i = 0
      var b = true
      while (i < N) {
        b = b ^ l.contains(i)
        i += 10000
      }
      b
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

    def array_reverse(a: ResizableArray[Int]): ResizableArray[Int] = {
      a.reverse
      a
    }

    def array_find(a: ResizableArray[Int]): Boolean = {
      var i = 0
      var b = true
      while (i < N) {
        b = b ^ a.contains(i) // TODO: Does this cost much?
        i += 10000
      }
      b
    }
  }

  import TestArray._
  import TestList._

  System.gc

  println("TESTING SPECIALIZED")
  val prefix = "specialized "

  var a: ResizableArray[Int] = null
  var b: Boolean = true
  test(prefix + "array", "insert ", _ => (),                 a = array_insert(),   () => { assert(a.length == N); a = null })
  test(prefix + "array", "reverse", _ => a = array_insert(), a = array_reverse(a), () => { assert(a.length == N); a = null })
  test(prefix + "array", "find   ", _ => a = array_insert(), b = array_find(a),    () => { assert(b == true); a = null })

  var l: List[Int] = null
  var i: Int = 0
  test(prefix + "list", "insert  ", _ => (),                 l = list_insert(),    () => { assert(l.length == N); l = null })
  test(prefix + "list", "hashCode", _ => l = list_insert(),  i = list_hashCode(l), () => { assert(i != 0); l = null })
  test(prefix + "list", "find    ", _ => l = list_insert(),  b = list_find(l),     () => { assert(b == true); l = null })
}
