package miniboxing.benchmarks.launch.tests

import org.scalameter.api._
import miniboxing.runtime.MiniboxConstants._
import miniboxing.runtime.MiniboxConversions._
import miniboxing.benchmarks.hardcoded._

trait HardcodedMiniboxingBenchTest extends BaseTest {

  private[this] object TestList {
    def list_insert(tag: Byte = INT): MBList[Int] = {
      var l: MBList[Int] = null
      var i = 0
      while (i < testSize) {
        l = new MBList_J[Int](IntToMinibox(i), l, tag)
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
      while (i < testSize) {
        b = b ^ l.contains_J(IntToMinibox(i))
        i += 10000
      }
      b
    }
  }

  private[this] object TestArray {
    def array_insert(tag: Byte = INT): MBResizableArray[Int] = {
      val a: MBResizableArray[Int] = new MBResizableArray_J[Int](tag)
      var i = 0
      while (i < testSize) {
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
      while (i < testSize) {
        b = b ^ a.contains_J(IntToMinibox(i)) // TODO: Does this cost much?
        i += 10000
      }
      b
    }
  }

  def testHardcodedMiniboxing(megamorphic: Boolean) = {
    import TestArray._
    import TestList._

    val transformation = "miniboxed standard " + (if (megamorphic) "mega" else "mono")

      def forceMegamorphicCallSites(): Unit =
      if (megamorphic) {
        def dirty(tag: Byte) = {
          array_find(array_reverse(array_insert(tag)))
          list_hashCode(list_insert(tag)); list_find(list_insert(tag))
        }
        dirty(INT)
        dirty(DOUBLE)
        dirty(LONG)
      }

    var a: MBResizableArray[Int] = null
    var b: Boolean = true
    test(transformation, "array.insert ", _ => { forceMegamorphicCallSites(); () },                 a = array_insert(),   () => { assert(a.length == testSize); a = null })
    test(transformation, "array.reverse", _ => { forceMegamorphicCallSites(); a = array_insert() }, a = array_reverse(a), () => { assert(a.length == testSize); a = null })
    test(transformation, "array.find   ", _ => { forceMegamorphicCallSites(); a = array_insert() }, b = array_find(a),    () => { assert(b == true); a = null })

    var l: MBList[Int] = null
    var i: Int = 0
    test(transformation, "list.insert  ", _ => { forceMegamorphicCallSites(); () },                 l = list_insert(),    () => { assert(l.length == testSize); l = null })
    test(transformation, "list.hashCode", _ => { forceMegamorphicCallSites(); l = list_insert() },  i = list_hashCode(l), () => { assert(i != 0); l = null })
    test(transformation, "list.find    ", _ => { forceMegamorphicCallSites(); l = list_insert() },  b = list_find(l),     () => { assert(b == true); l = null })
  }

}
