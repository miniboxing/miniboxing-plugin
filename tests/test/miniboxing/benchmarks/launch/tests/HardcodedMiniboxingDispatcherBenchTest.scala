package miniboxing.benchmarks.launch.tests

import org.scalameter.api._
import miniboxing.runtime.MiniboxConstants._
import miniboxing.runtime.MiniboxConversions._
import miniboxing.benchmarks.dispatcher._

trait HardcodedMiniboxingDispatcherBenchTest extends BaseTest {

  private[this] object TestList {
    def list_insert(dispatcher: Dispatcher[Int] = Dispatchers.IntDispatcher): DispList[Int] = {
      var l: DispList[Int] = null
      var i = 0
      while (i < testSize) {
        l = new DispList_J[Int](IntToMinibox(i), l, dispatcher)
        i += 1
      }
      l
    }

    def list_hashCode(list: DispList[Int]): Int = {
      list.hashCode_J
    }

    def list_find(l: DispList[Int]): Boolean = {
      var i = 0
      var b = true
      while (i < testSize) {
        b = b ^ l.contains_J(i.toLong)
        i += 10000
      }
      b
    }
  }

  private[this] object TestArray {
    def array_insert(dispatcher: Dispatcher[Int] = Dispatchers.IntDispatcher): DispResizableArray[Int] = {
      val a: DispResizableArray[Int] = new DispResizableArray_J[Int](dispatcher)
      var i = 0
      while (i < testSize) {
        a.add_J(IntToMinibox(i))
        i += 1
      }
      a
    }

    def array_reverse(a: DispResizableArray[Int]): DispResizableArray[Int] = {
      a.reverse_J
      a
    }

    def array_find(a: DispResizableArray[Int]): Boolean = {
      var i = 0
      var b = true
      while (i < testSize) {
        b = b ^ a.contains_J(IntToMinibox(i)) // TODO: Does this cost much?
        i += 10000
      }
      b
    }
  }


  def testHardcodedMiniboxingDispatch(megamorphic: Boolean) = {
    import TestArray._
    import TestList._

    val transformation = "miniboxed dispatch " + (if (megamorphic) "mega" else "mono")

    def forceMegamorphicCallSites(): Unit =
      if (megamorphic) {
        def dirty(dispatcher: Dispatcher[Int]) = {
          array_find(array_reverse(array_insert(dispatcher)))
          list_hashCode(list_insert(dispatcher)); list_find(list_insert(dispatcher))
        }
        dirty(Dispatchers.IntDispatcher)
        dirty(Dispatchers.DoubleDispatcher.asInstanceOf[Dispatcher[Int]])
        dirty(Dispatchers.LongDispatcher.asInstanceOf[Dispatcher[Int]])
      }

    var a: DispResizableArray[Int] = null
    var b: Boolean = true
    test(transformation, "array.insert ", _ => { forceMegamorphicCallSites(); () },                 a = array_insert(),   () => { assert(a.length == testSize); a = null })
    test(transformation, "array.reverse", _ => { forceMegamorphicCallSites(); a = array_insert() }, a = array_reverse(a), () => { assert(a.length == testSize); a = null })
    test(transformation, "array.find   ", _ => { forceMegamorphicCallSites(); a = array_insert() }, b = array_find(a),    () => { assert(b == true); a = null })

    var l: DispList[Int] = null
    var i: Int = 0
    test(transformation, "list.insert  ", _ => { forceMegamorphicCallSites(); () },                 l = list_insert(),    () => { assert(l.length == testSize); l = null })
    test(transformation, "list.hashCode", _ => { forceMegamorphicCallSites(); l = list_insert() },  i = list_hashCode(l), () => { assert(i != 0); l = null })
    test(transformation, "list.find    ", _ => { forceMegamorphicCallSites(); l = list_insert() },  b = list_find(l),     () => { assert(b == true); l = null })
  }
}
