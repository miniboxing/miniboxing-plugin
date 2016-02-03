package miniboxing.benchmarks.launch.tests

import org.scalameter.api._
import miniboxing.benchmarks.ideal._

trait IdealBenchTest extends BaseTest {

  private[this] object TestList {
    def list_insert(): List = {
      var l: List = null
      var i = 0
      while (i < testSize) {
        l = new List(i, l)
        i += 1
      }
      l
    }

    def list_hashCode(list: List): Int = {
      list.hashCode
    }

    def list_find(l: List): Boolean = {
      var i = 0
      var b = true
      while (i < testSize) {
        b = b ^ l.contains(i)
        i += 10000
      }
      b
    }
  }

  private[this] object TestArray {
    def array_insert(): ResizableArray = {
      val a: ResizableArray = new ResizableArray()
      var i = 0
      while (i < testSize) {
        a.add(i)
        i += 1
      }
      a
    }

    def array_reverse(a: ResizableArray): ResizableArray = {
      a.reverse
      a
    }

    def array_find(a: ResizableArray): Boolean = {
      var i = 0
      var b = true
      while (i < testSize) {
        b = b ^ a.contains(i) // TODO: Does this cost much?
        i += 10000
      }
      b
    }
  }

  def testIdeal() = {
    import TestArray._
    import TestList._

    val transformation = "ideal"

    var a: ResizableArray = null
    var b: Boolean = true
    //test(transformation, "array.insert ", _ => (),                 a = array_insert(),   () => { assert(a.length == testSize); a = null })
    //test(transformation, "array.reverse", _ => a = array_insert(), a = array_reverse(a), () => { assert(a.length == testSize); a = null })
    //test(transformation, "array.find   ", _ => a = array_insert(), b = array_find(a),    () => { assert(b == true); a = null })

    var l: List = null
    var i: Int = 0
    test(transformation, "list.insert  ", _ => (),                 l = list_insert(),    () => { assert(l.length == testSize); l = null })
    //test(transformation, "list.hashCode", _ => l = list_insert(),  i = list_hashCode(l), () => { assert(i != 0); l = null })
    //test(transformation, "list.find    ", _ => l = list_insert(),  b = list_find(l),     () => { assert(b == true); l = null })
  }
}
