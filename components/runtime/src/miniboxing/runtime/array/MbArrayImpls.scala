package miniboxing.runtime
package array

class MbArray_L[T](array: Array[AnyRef]) extends MbArray[T] {
  def this(array: Array[T]) =
    this({
      val array2 = new Array[AnyRef](array.length)
      var index = 0
      while (index < array.length) {
        array2(index) = array(index).asInstanceOf[AnyRef]
        index += 1
      }
      array2
    })

  def apply(idx: Int): T = array(idx).asInstanceOf[T]
  def update(idx: Int, value: T): Unit = array(idx) = value.asInstanceOf[AnyRef]
  override def clone: MbArray[T] = new MbArray_L[T](array.clone())
  def length: Int = array.length
}

class MbArray_J[T](T_Tag: Byte, array: Array[Long]) extends MbArray[T] {
  def this(T_Tag: Byte, array: Array[T]) =
    this(T_Tag, {
      val array2 = new Array[Long](array.length)
      var index = 0
      while (index < array.length) {
        array2(index) = MiniboxConversionsLong.box2minibox_tt(array(index), T_Tag)
        index += 1
      }
      array2
    })

  def apply(idx: Int): T = MiniboxConversionsLong.minibox2box(array(idx), T_Tag)
  def update(idx: Int, value: T): Unit = array(idx) = MiniboxConversionsLong.box2minibox_tt(value, T_Tag)
  override def clone: MbArray[T] = new MbArray_J[T](T_Tag, array.clone())
  def length: Int = array.length

  // optimized accessors:
  def apply_J(idx: Int): Long = array(idx)
  def update_J(idx: Int, value: Long): Unit = array(idx) = value
}

class MbArray_D[T](T_Tag: Byte, array: Array[Double]) extends MbArray[T] {
  def this(T_Tag: Byte, array: Array[T]) =
    this(T_Tag, {
      val array2 = new Array[Double](array.length)
      var index = 0
      while (index < array.length) {
        array2(index) = MiniboxConversionsDouble.box2minibox_tt(array(index), T_Tag)
        index += 1
      }
      array2
    })

  def apply(idx: Int): T = MiniboxConversionsDouble.minibox2box(array(idx), T_Tag)
  def update(idx: Int, value: T): Unit = array(idx) = MiniboxConversionsDouble.box2minibox_tt(value, T_Tag)
  override def clone: MbArray[T] = new MbArray_D[T](T_Tag, array.clone())
  def length: Int = array.length

  // optimized accessors:
  def apply_D(idx: Int): Double = array(idx)
  def update(idx: Int, value: Double): Unit = array(idx) = value
}