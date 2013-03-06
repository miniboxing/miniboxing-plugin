package miniboxing.benchmarks.launch.tests

import org.scalameter.api._
import miniboxing.runtime.MiniboxConstants._
import miniboxing.runtime.MiniboxConversions._
import miniboxing.benchmarks.dispatcher._

trait HardcodedMiniboxingDispatcherBenchTestCL extends BaseTest {

  private[this] object TestList {
    def list_insert(): DispList[Int] = {
      var l: DispList[Int] = null
      var i = 0
      while (i < testSize) {
        l = DispListFactory.newDispList_J[Int](IntToMinibox(i), l, Dispatchers.IntDispatcher)
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

    def list_insert_LONG(): DispList[Long] = {
      var l: DispList[Long] = null
      var i = 0
      while (i < testSize) {
        l = DispListFactory.newDispList_J[Long](LongToMinibox(i), l, Dispatchers.LongDispatcher)
        i += 1
      }
      l
    }

    def list_hashCode_LONG(list: DispList[Long]): Int = {
      list.hashCode_J
    }

    def list_find_LONG(l: DispList[Long]): Boolean = {
      var i = 0
      var b = true
      while (i < testSize) {
        b = b ^ l.contains_J(i.toLong)
        i += 10000
      }
      b
    }

    def list_insert_DOUBLE(): DispList[Double] = {
      var l: DispList[Double] = null
      var i = 0
      while (i < testSize) {
        l = DispListFactory.newDispList_J[Double](IntToMinibox(i), l, Dispatchers.DoubleDispatcher)
        i += 1
      }
      l
    }

    def list_hashCode_DOUBLE(list: DispList[Double]): Int = {
      list.hashCode_J
    }

    def list_find_DOUBLE(l: DispList[Double]): Boolean = {
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
    def array_insert(): DispResizableArray[Int] = {
      val a: DispResizableArray[Int] = DispResizableArrayFactory.newDispResizableArray_J[Int](Dispatchers.IntDispatcher)
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

    def array_insert_LONG(): DispResizableArray[Long] = {
      val a: DispResizableArray[Long] = DispResizableArrayFactory.newDispResizableArray_J[Long](Dispatchers.LongDispatcher)
      var i = 0
      while (i < testSize) {
        a.add_J(IntToMinibox(i))
        i += 1
      }
      a
    }

    def array_reverse_LONG(a: DispResizableArray[Long]): DispResizableArray[Long] = {
      a.reverse_J
      a
    }

    def array_find_LONG(a: DispResizableArray[Long]): Boolean = {
      var i = 0
      var b = true
      while (i < testSize) {
        b = b ^ a.contains_J(LongToMinibox(i)) // TODO: Does this cost much?
        i += 10000
      }
      b
    }

    def array_insert_DOUBLE(): DispResizableArray[Double] = {
      val a: DispResizableArray[Double] = DispResizableArrayFactory.newDispResizableArray_J[Double](Dispatchers.DoubleDispatcher)
      var i = 0
      while (i < testSize) {
        a.add_J(IntToMinibox(i))
        i += 1
      }
      a
    }

    def array_reverse_DOUBLE(a: DispResizableArray[Double]): DispResizableArray[Double] = {
      a.reverse_J
      a
    }

    def array_find_DOUBLE(a: DispResizableArray[Double]): Boolean = {
      var i = 0
      var b = true
      while (i < testSize) {
        b = b ^ a.contains_J(DoubleToMinibox(i)) // TODO: Does this cost much?
        i += 10000
      }
      b
    }
  }


  def testHardcodedMiniboxingDispatchClassLoader(megamorphic: Boolean) = {
    import TestArray._
    import TestList._

    val transformation = "miniboxed dispatch w/cl " + (if (megamorphic) "mega" else "mono")

    def forceDispInit() = {
      // whether or not it's megamorphic, let's stress test the inline caches:
      // let's make sure all three subclasees or Dispatcher are loaded in the system
//      println(Dispatchers.LongDispatcher)
//      println(Dispatchers.IntDispatcher)
//      println(Dispatchers.DoubleDispatcher)
    }

    def forceMegamorphicCallSites(): Unit =
      if (megamorphic) {
        withTestSize(1000) {
          array_find(array_reverse(array_insert()))
          list_hashCode(list_insert()); list_find(list_insert())
          array_find_LONG(array_reverse_LONG(array_insert_LONG()))
          list_hashCode_LONG(list_insert_LONG()); list_find_LONG(list_insert_LONG())
          array_find_DOUBLE(array_reverse_DOUBLE(array_insert_DOUBLE()))
          list_hashCode_DOUBLE(list_insert_DOUBLE()); list_find_DOUBLE(list_insert_DOUBLE())
        }
      }

    var a: DispResizableArray[Int] = null
    var b: Boolean = true
    test(transformation, "array.insert ", _ => { forceMegamorphicCallSites(); forceDispInit(); () },                 a = array_insert(),   () => { assert(a.length == testSize); a = null })
    test(transformation, "array.reverse", _ => { forceMegamorphicCallSites(); forceDispInit(); a = array_insert() }, a = array_reverse(a), () => { assert(a.length == testSize); a = null })
    test(transformation, "array.find   ", _ => { forceMegamorphicCallSites(); forceDispInit(); a = array_insert() }, b = array_find(a),    () => { assert(b == true); a = null })

    var l: DispList[Int] = null
    var i: Int = 0
    test(transformation, "list.insert  ", _ => { forceMegamorphicCallSites(); forceDispInit(); () },                 l = list_insert(),    () => { assert(l.length == testSize); l = null })
    test(transformation, "list.hashCode", _ => { forceMegamorphicCallSites(); forceDispInit(); l = list_insert() },  i = list_hashCode(l), () => { assert(i != 0); l = null })
    test(transformation, "list.find    ", _ => { forceMegamorphicCallSites(); forceDispInit(); l = list_insert() },  b = list_find(l),     () => { assert(b == true); l = null })
  }
}
