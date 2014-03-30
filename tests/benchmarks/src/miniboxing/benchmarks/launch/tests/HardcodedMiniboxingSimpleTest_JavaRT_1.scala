package miniboxing.benchmarks.launch.tests

import org.scalameter.api._
import miniboxing.runtime.MiniboxConstants._
import miniboxing.runtime.MiniboxConversions._
import miniboxing.benchmarks.hardcoded.java_rt_1._

trait HardcodedMiniboxingSimple_JavaRT_1 extends BaseTest {

  private[this] object TestList {
    def list_insert(): MBList[Int] = {
      var l: MBList[Int] = null
      var i = 0
      while (i < testSize) {
        l = new MBList_class_J[Int](int2minibox(i), l, INT)
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
        b = b ^ l.contains_J(int2minibox(i), INT)
        i += 10000
      }
      b
    }

    def list_insert_LONG(): MBList[Long] = {
      var l: MBList[Long] = null
      var i = 0
      while (i < testSize) {
        l = new MBList_class_J[Long](int2minibox(i), l, LONG)
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
    def list_insert_SHORT(): MBList[Short] = {
      var l: MBList[Short] = null
      var i = 0
      while (i < testSize) {
        l = new MBList_class_J[Short](int2minibox(i), l, SHORT)
        i += 1
      }
      l
    }

    def list_hashCode_SHORT(list: MBList[Short]): Int = {
      list.hashCode_J
    }

    def list_find_SHORT(l: MBList[Short]): Boolean = {
      var i = 0
      var b = true
      while (i < testSize) {
        b = b ^ l.contains_J(int2minibox(i), SHORT)
        i += 10000
      }
      b
    }
  }
  private[this] object TestArray {
    def array_insert(): MBResizableArray[Int] = {
      val a: MBResizableArray[Int] = new MBResizableArray_class_J[Int](INT)
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
      val a: MBResizableArray[Long] = new MBResizableArray_class_J[Long](LONG)
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

    def array_insert_SHORT(): MBResizableArray[Short] = {
      val a: MBResizableArray[Short] = new MBResizableArray_class_J[Short](SHORT)
      var i = 0
      while (i < testSize) {
        a.add_J(int2minibox(i), SHORT)
        i += 1
      }
      a
    }

    def array_reverse_SHORT(a: MBResizableArray[Short]): MBResizableArray[Short] = {
      a.reverse_J
      a
    }

    def array_find_SHORT(a: MBResizableArray[Short]): Boolean = {
      var i = 0
      var b = true
      while (i < testSize) {
        b = b ^ a.contains_J(int2minibox(i), SHORT) // TODO: Does this cost much?
        i += 10000
      }
      b
    }
  }

  def testHardcodedMiniboxingSimple_JavaRT_1(megamorphic: Boolean, jvmFlags: List[String]) = {
    import TestArray._
    import TestList._

    val transformation = "miniboxed JavaRT 1" + (if (megamorphic) "mega" else "mono") + (if (jvmFlags.nonEmpty) " +f" else " -f")

    def forceMegamorphicCallSites(): Unit =
      if (megamorphic) {
        withTestSize(megamorphicTestSize) {
          array_find(array_reverse(array_insert()))
          list_hashCode(list_insert()); list_find(list_insert())
          array_find_LONG(array_reverse_LONG(array_insert_LONG()))
          list_hashCode_LONG(list_insert_LONG()); list_find_LONG(list_insert_LONG())
          array_find_SHORT(array_reverse_SHORT(array_insert_SHORT()))
          list_hashCode_SHORT(list_insert_SHORT()); list_find_SHORT(list_insert_SHORT())
        }
      }

    var a: MBResizableArray[Int] = null
    var b: Boolean = true
    test(transformation, "array.insert ", _ => { forceMegamorphicCallSites(); () },                 a = array_insert(),   () => { assert(a.length == testSize); a = null }, jvmFlags)
    test(transformation, "array.reverse", _ => { forceMegamorphicCallSites(); a = array_insert() }, a = array_reverse(a), () => { assert(a.length == testSize); a = null }, jvmFlags)
    test(transformation, "array.find   ", _ => { forceMegamorphicCallSites(); a = array_insert() }, b = array_find(a),    () => { assert(b == true); a = null }, jvmFlags)

    var l: MBList[Int] = null
    var i: Int = 0
    test(transformation, "list.insert  ", _ => { forceMegamorphicCallSites(); () },                 l = list_insert(),    () => { assert(l.length == testSize); l = null }, jvmFlags)
    test(transformation, "list.hashCode", _ => { forceMegamorphicCallSites(); l = list_insert() },  i = list_hashCode(l), () => { assert(i != 0); l = null }, jvmFlags)
    test(transformation, "list.find    ", _ => { forceMegamorphicCallSites(); l = list_insert() },  b = list_find(l),     () => { assert(b == true); l = null }, jvmFlags)
  }

}
