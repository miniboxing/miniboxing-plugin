package miniboxing.benchmarks.launch.tests

import org.scalameter.api._
import miniboxing.runtime.MiniboxConstants._
import miniboxing.runtime.MiniboxConversions._
import miniboxing.benchmarks.hardcoded.fullswitch._

trait HardcodedMiniboxingSimpleCL extends BaseTest {

  private[this] object TestList {
    def list_insert(): MBList[Int] = {
      var l: MBList[Int] = null
      var i = 0
      while (i < testSize) {
        l = MBListFactory.newMBList_J[Int](int2minibox(i), l, INT)
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
        b = b ^ l.contains_J(i.toLong, INT)
        i += 10000
      }
      b
    }

    def list_insert_DOUBLE(): MBList[Double] = {
      var l: MBList[Double] = null
      var i = 0
      while (i < testSize) {
        l = MBListFactory.newMBList_J[Double](int2minibox(i), l, DOUBLE)
        i += 1
      }
      l
    }

    def list_hashCode_DOUBLE(list: MBList[Double]): Int = {
      list.hashCode_J
    }

    def list_find_DOUBLE(l: MBList[Double]): Boolean = {
      var i = 0
      var b = true
      while (i < testSize) {
        b = b ^ l.contains_J(int2minibox(i), DOUBLE)
        i += 10000
      }
      b
    }

    def list_insert_LONG(): MBList[Long] = {
      var l: MBList[Long] = null
      var i = 0
      while (i < testSize) {
        l = MBListFactory.newMBList_J[Long](int2minibox(i), l, LONG)
        i += 1
      }
      l
    }

    def list_hashCode_LONG(list: MBList[Long]): Int = {
      list.hashCode_J
    }

    def list_find_LONG(l: MBList[Long]): Boolean = {
      var i = 0
      var b = true
      while (i < testSize) {
        b = b ^ l.contains_J(int2minibox(i), LONG)
        i += 10000
      }
      b
    }
  }

  private[this] object TestArray {
    def array_insert(): MBResizableArray[Int] = {
      val a: MBResizableArray[Int] = MBResizableArrayFactory.newMBResizableArray_J[Int](INT)
      var i = 0
      while (i < testSize) {
        a.add_J(int2minibox(i), INT)
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
        b = b ^ a.contains_J(int2minibox(i), INT) // TODO: Does this cost much?
        i += 10000
      }
      b
    }

      def array_insert_LONG(): MBResizableArray[Long] = {
      val a: MBResizableArray[Long] = MBResizableArrayFactory.newMBResizableArray_J[Long](LONG)
      var i = 0
      while (i < testSize) {
        a.add_J(long2minibox(i), LONG)
        i += 1
      }
      a
    }

    def array_reverse_LONG(a: MBResizableArray[Long]): MBResizableArray[Long] = {
      a.reverse_J
      a
    }

    def array_find_LONG(a: MBResizableArray[Long]): Boolean = {
      var i = 0
      var b = true
      while (i < testSize) {
        b = b ^ a.contains_J(long2minibox(i), LONG) // TODO: Does this cost much?
        i += 10000
      }
      b
    }

    def array_insert_DOUBLE(): MBResizableArray[Double] = {
      val a: MBResizableArray[Double] = MBResizableArrayFactory.newMBResizableArray_J[Double](DOUBLE)
      var i = 0
      while (i < testSize) {
        a.add_J(double2minibox(i), DOUBLE)
        i += 1
      }
      a
    }

    def array_reverse_DOUBLE(a: MBResizableArray[Double]): MBResizableArray[Double] = {
      a.reverse_J
      a
    }

    def array_find_DOUBLE(a: MBResizableArray[Double]): Boolean = {
      var i = 0
      var b = true
      while (i < testSize) {
        b = b ^ a.contains_J(double2minibox(i), DOUBLE) // TODO: Does this cost much?
        i += 10000
      }
      b
    }
  }

  def testHardcodedMiniboxingSimpleClassLoader(megamorphic: Boolean) = {
    import TestArray._
    import TestList._

    val transformation = "miniboxed standard w/cl " + (if (megamorphic) "mega" else "mono")

    def forceMegamorphicCallSites(): Unit =
      if (megamorphic) {
        withTestSize(megamorphicTestSize) {
          array_find(array_reverse(array_insert()))
          list_hashCode(list_insert()); list_find(list_insert())
          array_find_LONG(array_reverse_LONG(array_insert_LONG()))
          list_hashCode_LONG(list_insert_LONG()); list_find_LONG(list_insert_LONG())
          array_find_DOUBLE(array_reverse_DOUBLE(array_insert_DOUBLE()))
          list_hashCode_DOUBLE(list_insert_DOUBLE()); list_find_DOUBLE(list_insert_DOUBLE())
        }
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
