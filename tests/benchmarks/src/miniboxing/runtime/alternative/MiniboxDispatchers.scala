package miniboxing.runtime.alternative

import miniboxing.internal.MiniboxConstants.DOUBLE
import miniboxing.internal.MiniboxConstants.INT
import miniboxing.internal.MiniboxConstants.LONG
import miniboxing.internal.MiniboxConversions.double2minibox
import miniboxing.internal.MiniboxConversions.int2minibox
import miniboxing.internal.MiniboxConversions.long2minibox
import miniboxing.internal.MiniboxConversions.minibox2double
import miniboxing.internal.MiniboxConversions.minibox2int
import miniboxing.internal.MiniboxConversions.minibox2long

abstract class Dispatcher[T] {

  def tag: Int
  def mbarray_new(len: Int): Array[T]
  def mbarray_apply_minibox(array: Any, idx: Int): Long
  def mbarray_apply_box(array: Any, idx: Int): T
  def mbarray_update_minibox(array: Any, idx: Int, value: Long): Unit
  def mbarray_update_box(array: Any, idx: Int, value: T): Unit
  def mbarray_length(array: Any): Int
  def minibox2box(l: Long) : T
  def box2minibox(a: Any): Long
  def mboxed_toString(x: Long): String
  // TODO: Don't we want `def mboxed_eqeq(x: Long, y: Any)` instead?
  def mboxed_eqeq(x: Long, y: Long, ytag: Byte): Boolean
  def mboxed_eqeq(x: Long, y: Long): Boolean
  def mboxed_hashhash(x: Long): Int
  def mboxed_hashCode(x: Long): Int = mboxed_hashhash(x)
}

object Dispatchers {
  object IntDispatcher extends Dispatcher[Int] {
    import miniboxing.internal.MiniboxConstants._
    import miniboxing.internal.MiniboxConversions._

    def tag: Int = INT
    def mbarray_new(len: Int): Array[Int] = new Array[Int](len)
    def mbarray_apply_minibox(array: Any, idx: Int): Long = int2minibox(array.asInstanceOf[Array[Int]](idx))
    def mbarray_apply_box(array: Any, idx: Int): Int = array.asInstanceOf[Array[Int]](idx)
    def mbarray_update_minibox(array: Any, idx: Int, value: Long): Unit = array.asInstanceOf[Array[Int]](idx) = minibox2int(value)
    def mbarray_update_box(array: Any, idx: Int, value: Int): Unit = array.asInstanceOf[Array[Int]](idx) = value
    def mbarray_length(array: Any): Int = array.asInstanceOf[Array[Int]].length
    def minibox2box(l: Long): Int = minibox2int(l)
    def box2minibox(a: Any): Long = int2minibox(a.asInstanceOf[Int])
    def mboxed_toString(x: Long): String = minibox2int(x).toString
    def mboxed_eqeq(x: Long, y: Long, ytag: Byte): Boolean = if (ytag == INT) y == x else {
      import miniboxing.internal.MiniboxConversions
      MiniboxConversions.minibox2box(x, INT) == MiniboxConversions.minibox2box(y, ytag)
    }
    def mboxed_eqeq(x: Long, y: Long): Boolean = x == y
    def mboxed_hashhash(x: Long): Int = minibox2int(x).hashCode   // TODO: Is hashcode right?
  }

  object DoubleDispatcher extends Dispatcher[Double] {
    import miniboxing.internal.MiniboxConstants._
    import miniboxing.internal.MiniboxConversions._

    def tag: Int = DOUBLE
    def mbarray_new(len: Int): Array[Double] = new Array[Double](len)
    def mbarray_apply_minibox(array: Any, idx: Int): Long = double2minibox(array.asInstanceOf[Array[Double]](idx))
    def mbarray_apply_box(array: Any, idx: Int): Double = array.asInstanceOf[Array[Double]](idx)
    def mbarray_update_minibox(array: Any, idx: Int, value: Long): Unit = array.asInstanceOf[Array[Double]](idx) = minibox2double(value)
    def mbarray_update_box(array: Any, idx: Int, value: Double): Unit = array.asInstanceOf[Array[Double]](idx) = value
    def mbarray_length(array: Any): Int = array.asInstanceOf[Array[Double]].length
    def minibox2box(l: Long): Double = minibox2double(l)
    def box2minibox(a: Any): Long = double2minibox(a.asInstanceOf[Double])
    def mboxed_toString(x: Long): String = minibox2double(x).toString
    def mboxed_eqeq(x: Long, y: Long, ytag: Byte): Boolean = if (ytag == DOUBLE) y == x else {
      import miniboxing.internal.MiniboxConversions
      MiniboxConversions.minibox2box(x, DOUBLE) == MiniboxConversions.minibox2box(y, ytag)
    }
    def mboxed_eqeq(x: Long, y: Long): Boolean = x == y
    def mboxed_hashhash(x: Long): Int = minibox2double(x).hashCode   // TODO: Is hashcode right?
  }

  object LongDispatcher extends Dispatcher[Long] {
    import miniboxing.internal.MiniboxConstants._
    import miniboxing.internal.MiniboxConversions._

    def tag: Int = LONG
    def mbarray_new(len: Int): Array[Long] = new Array[Long](len)
    def mbarray_apply_minibox(array: Any, idx: Int): Long = long2minibox(array.asInstanceOf[Array[Long]](idx))
    def mbarray_apply_box(array: Any, idx: Int): Long = array.asInstanceOf[Array[Long]](idx)
    def mbarray_update_minibox(array: Any, idx: Int, value: Long): Unit = array.asInstanceOf[Array[Long]](idx) = minibox2long(value)
    def mbarray_update_box(array: Any, idx: Int, value: Long): Unit = array.asInstanceOf[Array[Long]](idx) = value
    def mbarray_length(array: Any): Int = array.asInstanceOf[Array[Long]].length
    def minibox2box(l: Long): Long = minibox2long(l)
    def box2minibox(a: Any): Long = long2minibox(a.asInstanceOf[Long])
    def mboxed_toString(x: Long): String = minibox2long(x).toString
    def mboxed_eqeq(x: Long, y: Long, ytag: Byte): Boolean = if (ytag == LONG) y == x else {
      import miniboxing.internal.MiniboxConversions
      MiniboxConversions.minibox2box(x, LONG) == MiniboxConversions.minibox2box(y, ytag)
    }
    def mboxed_eqeq(x: Long, y: Long): Boolean = x == y
    def mboxed_hashhash(x: Long): Int = minibox2long(x).hashCode   // TODO: Is hashcode right?
  }
}
