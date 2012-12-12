package benchmarks

import miniboxing.runtime._
import miniboxing.runtime.MiniboxConversions._

/**
 * The benchmarks used in the project are:
 *  - insert N elements in the data structure
 *  - reverse the elements
 *  - search 100 of them
 */
//object Main {
//  val N = 1000000
//  val M = 20
//
//  def list_insert() : MBList_J = {
//      var l : MBList_J = null
//      var i = 0
//      while (i < N) {
//        l = new MBList_J(IntToMinibox(i), l, MiniboxConstants.INT)
//        i += 1
//      }
//      l
//  }
//
//  def list_hashCode(list: MBList_J): Int = {
//    list.hashCode_J
//  }
//
//  def list_find(l: MBList_J) = {
//    var i = 0
//    while (i < N){
//      l contains_J (IntToMinibox(i))
//      i += 10000
//    }
//  }
//
//  def array_insert(): MBResizableArray_J = {
//    val a : MBResizableArray_J = new MBResizableArray_J(MiniboxConstants.INT)
//    var i = 0
//    while (i < N){
//      a add_J IntToMinibox(i)
//      i += 1
//    }
//    a
//  }
//
//  def array_reverse(a : MBResizableArray_J) = {
//    a.reverse_J
//  }
//
//  def array_find(a: MBResizableArray_J) = {
//    var i = 0
//    while (i < N){
//      a contains_J (IntToMinibox(i))
//      i += 10000
//    }
//  }
//
//
//  def main(args: Array[String]) {
//    val a = array_insert()
//    Benchmark.timed("array insert  ", array_insert(), 30)
//    Benchmark.timed("array_reverse ", array_reverse(a), 75)
//    Benchmark.timed("array_find    ", array_find(a), 3)
//
//    val l = list_insert()
//    Benchmark.timed("list_insert   ", list_insert(), 30)
//    Benchmark.timed("list_hashCode ", list_hashCode(l), 50)
//    Benchmark.timed("list_find     ", list_find(l), 1)
//
//  }
//}
