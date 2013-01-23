package miniboxing.benchmarks.dispatcher

abstract class Dispatcher[T] {
  import miniboxing.runtime.MiniboxTypes._
  def mbarray_new(len: Int): Any
  def mbarray_apply_minibox(array: Any, idx: Int): Minibox
  def mbarray_apply_box(array: Any, idx: Int): T
  def mbarray_update_minibox(array: Any, idx: Int, value: Minibox): Unit
  def mbarray_update_box(array: Any, idx: Int, value: T): Unit
  def mbarray_length(array: Any): Int
  def minibox2box(l: Minibox) : T
  def box2minibox(a: Any): Minibox
  def mboxed_toString(x: Minibox): String
  def mboxed_eqeq(x: Minibox, y: Minibox, ytag: Tag): Boolean
  def mboxed_eqeq(x: Minibox, y: Minibox): Boolean
  def mboxed_hashhash(x: Minibox): Int
  def mboxed_hashCode(x: Minibox): Int = mboxed_hashhash(x)
}

object Dispatchers {
  object IntDispatcher extends Dispatcher[Int] {
    import miniboxing.runtime.MiniboxTypes._
    import miniboxing.runtime.MiniboxConstants._
    import miniboxing.runtime.MiniboxConversions._

    def mbarray_new(len: Int): Any = new Array[Int](len)
    def mbarray_apply_minibox(array: Any, idx: Int): Minibox = IntToMinibox(array.asInstanceOf[Array[Int]](idx))
    def mbarray_apply_box(array: Any, idx: Int): Int = array.asInstanceOf[Array[Int]](idx)
    def mbarray_update_minibox(array: Any, idx: Int, value: Minibox): Unit = array.asInstanceOf[Array[Int]](idx) = MiniboxToInt(value)
    def mbarray_update_box(array: Any, idx: Int, value: Int): Unit = array.asInstanceOf[Array[Int]](idx) = value
    def mbarray_length(array: Any): Int = array.asInstanceOf[Array[Int]].length
    def minibox2box(l: Minibox): Int = MiniboxToInt(l)
    def box2minibox(a: Any): Minibox = IntToMinibox(a.asInstanceOf[Int])
    def mboxed_toString(x: Minibox): String = MiniboxToInt(x).toString
    def mboxed_eqeq(x: Minibox, y: Minibox, ytag: Tag): Boolean = if (ytag == INT) y == x else {
      import miniboxing.runtime.MiniboxConversions
      MiniboxConversions.minibox2box(x, INT) == MiniboxConversions.minibox2box(y, ytag)
    }
    def mboxed_eqeq(x: Minibox, y: Minibox): Boolean = x == y
    def mboxed_hashhash(x: Minibox): Int = MiniboxToInt(x).hashCode   // TODO: Is hashcode right?
  }

  object DoubleDispatcher extends Dispatcher[Double] {
    import miniboxing.runtime.MiniboxTypes._
    import miniboxing.runtime.MiniboxConstants._
    import miniboxing.runtime.MiniboxConversions._

    def mbarray_new(len: Int): Any = new Array[Double](len)
    def mbarray_apply_minibox(array: Any, idx: Int): Minibox = DoubleToMinibox(array.asInstanceOf[Array[Double]](idx))
    def mbarray_apply_box(array: Any, idx: Int): Double = array.asInstanceOf[Array[Double]](idx)
    def mbarray_update_minibox(array: Any, idx: Int, value: Minibox): Unit = array.asInstanceOf[Array[Double]](idx) = MiniboxToDouble(value)
    def mbarray_update_box(array: Any, idx: Int, value: Double): Unit = array.asInstanceOf[Array[Double]](idx) = value
    def mbarray_length(array: Any): Int = array.asInstanceOf[Array[Double]].length
    def minibox2box(l: Minibox): Double = MiniboxToDouble(l)
    def box2minibox(a: Any): Minibox = DoubleToMinibox(a.asInstanceOf[Double])
    def mboxed_toString(x: Minibox): String = MiniboxToDouble(x).toString
    def mboxed_eqeq(x: Minibox, y: Minibox, ytag: Tag): Boolean = if (ytag == DOUBLE) y == x else {
      import miniboxing.runtime.MiniboxConversions
      MiniboxConversions.minibox2box(x, DOUBLE) == MiniboxConversions.minibox2box(y, ytag)
    }
    def mboxed_eqeq(x: Minibox, y: Minibox): Boolean = x == y
    def mboxed_hashhash(x: Minibox): Int = MiniboxToDouble(x).hashCode   // TODO: Is hashcode right?
  }

  object LongDispatcher extends Dispatcher[Long] {
    import miniboxing.runtime.MiniboxTypes._
    import miniboxing.runtime.MiniboxConstants._
    import miniboxing.runtime.MiniboxConversions._

    def mbarray_new(len: Int): Any = new Array[Long](len)
    def mbarray_apply_minibox(array: Any, idx: Int): Minibox = LongToMinibox(array.asInstanceOf[Array[Long]](idx))
    def mbarray_apply_box(array: Any, idx: Int): Long = array.asInstanceOf[Array[Long]](idx)
    def mbarray_update_minibox(array: Any, idx: Int, value: Minibox): Unit = array.asInstanceOf[Array[Long]](idx) = MiniboxToLong(value)
    def mbarray_update_box(array: Any, idx: Int, value: Long): Unit = array.asInstanceOf[Array[Long]](idx) = value
    def mbarray_length(array: Any): Int = array.asInstanceOf[Array[Long]].length
    def minibox2box(l: Minibox): Long = MiniboxToLong(l)
    def box2minibox(a: Any): Minibox = LongToMinibox(a.asInstanceOf[Long])
    def mboxed_toString(x: Minibox): String = MiniboxToLong(x).toString
    def mboxed_eqeq(x: Minibox, y: Minibox, ytag: Tag): Boolean = if (ytag == LONG) y == x else {
      import miniboxing.runtime.MiniboxConversions
      MiniboxConversions.minibox2box(x, LONG) == MiniboxConversions.minibox2box(y, ytag)
    }
    def mboxed_eqeq(x: Minibox, y: Minibox): Boolean = x == y
    def mboxed_hashhash(x: Minibox): Int = MiniboxToLong(x).hashCode   // TODO: Is hashcode right?
  }
}
