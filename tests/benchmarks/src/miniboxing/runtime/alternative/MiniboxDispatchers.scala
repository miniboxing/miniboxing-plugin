package miniboxing.runtime.alternative

import miniboxing.runtime.MiniboxConstants.DOUBLE
import miniboxing.runtime.MiniboxConstants.INT
import miniboxing.runtime.MiniboxConstants.LONG
import miniboxing.runtime.MiniboxConversions.DoubleToMinibox
import miniboxing.runtime.MiniboxConversions.IntToMinibox
import miniboxing.runtime.MiniboxConversions.LongToMinibox
import miniboxing.runtime.MiniboxConversions.MiniboxToDouble
import miniboxing.runtime.MiniboxConversions.MiniboxToInt
import miniboxing.runtime.MiniboxConversions.MiniboxToLong

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
    import miniboxing.runtime.MiniboxConstants._
    import miniboxing.runtime.MiniboxConversions._

    def tag: Int = INT
    def mbarray_new(len: Int): Array[Int] = new Array[Int](len)
    def mbarray_apply_minibox(array: Any, idx: Int): Long = IntToMinibox(array.asInstanceOf[Array[Int]](idx))
    def mbarray_apply_box(array: Any, idx: Int): Int = array.asInstanceOf[Array[Int]](idx)
    def mbarray_update_minibox(array: Any, idx: Int, value: Long): Unit = array.asInstanceOf[Array[Int]](idx) = MiniboxToInt(value)
    def mbarray_update_box(array: Any, idx: Int, value: Int): Unit = array.asInstanceOf[Array[Int]](idx) = value
    def mbarray_length(array: Any): Int = array.asInstanceOf[Array[Int]].length
    def minibox2box(l: Long): Int = MiniboxToInt(l)
    def box2minibox(a: Any): Long = IntToMinibox(a.asInstanceOf[Int])
    def mboxed_toString(x: Long): String = MiniboxToInt(x).toString
    def mboxed_eqeq(x: Long, y: Long, ytag: Byte): Boolean = if (ytag == INT) y == x else {
      import miniboxing.runtime.MiniboxConversions
      MiniboxConversions.minibox2box(x, INT) == MiniboxConversions.minibox2box(y, ytag)
    }
    def mboxed_eqeq(x: Long, y: Long): Boolean = x == y
    def mboxed_hashhash(x: Long): Int = MiniboxToInt(x).hashCode   // TODO: Is hashcode right?
  }

  object DoubleDispatcher extends Dispatcher[Double] {
    import miniboxing.runtime.MiniboxConstants._
    import miniboxing.runtime.MiniboxConversions._

    def tag: Int = DOUBLE
    def mbarray_new(len: Int): Array[Double] = new Array[Double](len)
    def mbarray_apply_minibox(array: Any, idx: Int): Long = DoubleToMinibox(array.asInstanceOf[Array[Double]](idx))
    def mbarray_apply_box(array: Any, idx: Int): Double = array.asInstanceOf[Array[Double]](idx)
    def mbarray_update_minibox(array: Any, idx: Int, value: Long): Unit = array.asInstanceOf[Array[Double]](idx) = MiniboxToDouble(value)
    def mbarray_update_box(array: Any, idx: Int, value: Double): Unit = array.asInstanceOf[Array[Double]](idx) = value
    def mbarray_length(array: Any): Int = array.asInstanceOf[Array[Double]].length
    def minibox2box(l: Long): Double = MiniboxToDouble(l)
    def box2minibox(a: Any): Long = DoubleToMinibox(a.asInstanceOf[Double])
    def mboxed_toString(x: Long): String = MiniboxToDouble(x).toString
    def mboxed_eqeq(x: Long, y: Long, ytag: Byte): Boolean = if (ytag == DOUBLE) y == x else {
      import miniboxing.runtime.MiniboxConversions
      MiniboxConversions.minibox2box(x, DOUBLE) == MiniboxConversions.minibox2box(y, ytag)
    }
    def mboxed_eqeq(x: Long, y: Long): Boolean = x == y
    def mboxed_hashhash(x: Long): Int = MiniboxToDouble(x).hashCode   // TODO: Is hashcode right?
  }

  object LongDispatcher extends Dispatcher[Long] {
    import miniboxing.runtime.MiniboxConstants._
    import miniboxing.runtime.MiniboxConversions._

    def tag: Int = LONG
    def mbarray_new(len: Int): Array[Long] = new Array[Long](len)
    def mbarray_apply_minibox(array: Any, idx: Int): Long = LongToMinibox(array.asInstanceOf[Array[Long]](idx))
    def mbarray_apply_box(array: Any, idx: Int): Long = array.asInstanceOf[Array[Long]](idx)
    def mbarray_update_minibox(array: Any, idx: Int, value: Long): Unit = array.asInstanceOf[Array[Long]](idx) = MiniboxToLong(value)
    def mbarray_update_box(array: Any, idx: Int, value: Long): Unit = array.asInstanceOf[Array[Long]](idx) = value
    def mbarray_length(array: Any): Int = array.asInstanceOf[Array[Long]].length
    def minibox2box(l: Long): Long = MiniboxToLong(l)
    def box2minibox(a: Any): Long = LongToMinibox(a.asInstanceOf[Long])
    def mboxed_toString(x: Long): String = MiniboxToLong(x).toString
    def mboxed_eqeq(x: Long, y: Long, ytag: Byte): Boolean = if (ytag == LONG) y == x else {
      import miniboxing.runtime.MiniboxConversions
      MiniboxConversions.minibox2box(x, LONG) == MiniboxConversions.minibox2box(y, ytag)
    }
    def mboxed_eqeq(x: Long, y: Long): Boolean = x == y
    def mboxed_hashhash(x: Long): Int = MiniboxToLong(x).hashCode   // TODO: Is hashcode right?
  }
}
