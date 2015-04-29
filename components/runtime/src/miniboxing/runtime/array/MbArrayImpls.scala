package miniboxing.runtime
package array

class MbArray_L[T](private[array] final val array: Array[AnyRef]) extends MbArray[T] {

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

  def this(size: Int) =
    this(new Array[AnyRef](size))

  def apply(idx: Int): T = array(idx).asInstanceOf[T]
  def update(idx: Int, value: T): Unit = array(idx) = value.asInstanceOf[AnyRef]
  override def clone: MbArray[T] = new MbArray_L[T](array.clone())
  def length: Int = array.length

  override def arraycopy(srcPos: Int, dest: MbArray[T], destPos: Int, length: Int) = {
    dest match {
      case dest_L: MbArray_L[T] => System.arraycopy(array, srcPos, dest_L.array, destPos, length)
      case _ => super.arraycopy(srcPos, dest, destPos, length)
    }
  }

}

class MbArray_I[T](private[array] final val array: Array[Int]) extends MbArray[T] {

  def this(array: Array[T]) =
    this({
      val array2 = new Array[Int](array.length)
      var index = 0
      while (index < array.length) {
        array2(index) = array(index).asInstanceOf[Int]
        index += 1
      }
      array2
    })

  def this(size: Int) =
    this(new Array[Int](size))

  def apply(idx: Int): T = array(idx).asInstanceOf[T]
  def update(idx: Int, value: T): Unit = array(idx) = value.asInstanceOf[Int]
  override def clone: MbArray[T] = new MbArray_I[T](array.clone())
  def length: Int = array.length

  // optimized accessors:
  def apply_J(idx: Int): Long = MiniboxConversions.int2minibox(array(idx))
  def update_J(idx: Int, value: Long): Unit = array(idx) = MiniboxConversions.minibox2int(value)
  override def arraycopy(srcPos: Int, dest: MbArray[T], destPos: Int, length: Int) = {
    dest match {
      case dest_J: MbArray_I[T] => System.arraycopy(array, srcPos, dest_J.array, destPos, length)
      case _ => super.arraycopy(srcPos, dest, destPos, length)
    }
  }
}

class MbArray_S[T](private[array] final val array: Array[Short]) extends MbArray[T] {

  def this(array: Array[T]) =
    this({
      val array2 = new Array[Short](array.length)
      var index = 0
      while (index < array.length) {
        array2(index) = array(index).asInstanceOf[Short]
        index += 1
      }
      array2
    })

  def this(size: Int) =
    this(new Array[Short](size))

  def apply(idx: Int): T = array(idx).asInstanceOf[T]
  def update(idx: Int, value: T): Unit = array(idx) = value.asInstanceOf[Short]
  override def clone: MbArray[T] = new MbArray_S[T](array.clone())
  def length: Int = array.length

  // optimized accessors:
  def apply_J(idx: Int): Long = MiniboxConversions.short2minibox(array(idx))
  def update_J(idx: Int, value: Long): Unit = array(idx) = MiniboxConversions.minibox2short(value)
  override def arraycopy(srcPos: Int, dest: MbArray[T], destPos: Int, length: Int) = {
    dest match {
      case dest_J: MbArray_S[T] => System.arraycopy(array, srcPos, dest_J.array, destPos, length)
      case _ => super.arraycopy(srcPos, dest, destPos, length)
    }
  }
}

class MbArray_B[T](private[array] final val array: Array[Byte]) extends MbArray[T] {

  def this(array: Array[T]) =
    this({
      val array2 = new Array[Byte](array.length)
      var index = 0
      while (index < array.length) {
        array2(index) = array(index).asInstanceOf[Byte]
        index += 1
      }
      array2
    })

  def this(size: Int) =
    this(new Array[Byte](size))

  def apply(idx: Int): T = array(idx).asInstanceOf[T]
  def update(idx: Int, value: T): Unit = array(idx) = value.asInstanceOf[Byte]
  override def clone: MbArray[T] = new MbArray_B[T](array.clone())
  def length: Int = array.length

  // optimized accessors:
  def apply_J(idx: Int): Long = MiniboxConversions.byte2minibox(array(idx))
  def update_J(idx: Int, value: Long): Unit = array(idx) = MiniboxConversions.minibox2byte(value)
  override def arraycopy(srcPos: Int, dest: MbArray[T], destPos: Int, length: Int) = {
    dest match {
      case dest_J: MbArray_B[T] => System.arraycopy(array, srcPos, dest_J.array, destPos, length)
      case _ => super.arraycopy(srcPos, dest, destPos, length)
    }
  }
}

class MbArray_Z[T](private[array] final val array: Array[Boolean]) extends MbArray[T] {

  def this(array: Array[T]) =
    this({
      val array2 = new Array[Boolean](array.length)
      var index = 0
      while (index < array.length) {
        array2(index) = array(index).asInstanceOf[Boolean]
        index += 1
      }
      array2
    })

  def this(size: Int) =
    this(new Array[Boolean](size))

  def apply(idx: Int): T = array(idx).asInstanceOf[T]
  def update(idx: Int, value: T): Unit = array(idx) = value.asInstanceOf[Boolean]
  override def clone: MbArray[T] = new MbArray_Z[T](array.clone())
  def length: Int = array.length

  // optimized accessors:
  def apply_J(idx: Int): Long = MiniboxConversions.boolean2minibox(array(idx))
  def update_J(idx: Int, value: Long): Unit = array(idx) = MiniboxConversions.minibox2boolean(value)
  override def arraycopy(srcPos: Int, dest: MbArray[T], destPos: Int, length: Int) = {
    dest match {
      case dest_J: MbArray_Z[T] => System.arraycopy(array, srcPos, dest_J.array, destPos, length)
      case _ => super.arraycopy(srcPos, dest, destPos, length)
    }
  }
}

class MbArray_C[T](private[array] final val array: Array[Char]) extends MbArray[T] {

  def this(array: Array[T]) =
    this({
      val array2 = new Array[Char](array.length)
      var index = 0
      while (index < array.length) {
        array2(index) = array(index).asInstanceOf[Char]
        index += 1
      }
      array2
    })

  def this(size: Int) =
    this(new Array[Char](size))

  def apply(idx: Int): T = array(idx).asInstanceOf[T]
  def update(idx: Int, value: T): Unit = array(idx) = value.asInstanceOf[Char]
  override def clone: MbArray[T] = new MbArray_C[T](array.clone())
  def length: Int = array.length

  // optimized accessors:
  def apply_J(idx: Int): Long = MiniboxConversions.char2minibox(array(idx))
  def update_J(idx: Int, value: Long): Unit = array(idx) = MiniboxConversions.minibox2char(value)
  override def arraycopy(srcPos: Int, dest: MbArray[T], destPos: Int, length: Int) = {
    dest match {
      case dest_J: MbArray_C[T] => System.arraycopy(array, srcPos, dest_J.array, destPos, length)
      case _ => super.arraycopy(srcPos, dest, destPos, length)
    }
  }
}

class MbArray_V[T](private[array] final val array: Array[Unit]) extends MbArray[T] {

  def this(array: Array[T]) =
    this({
      val array2 = new Array[Unit](array.length)
      var index = 0
      while (index < array.length) {
        array2(index) = array(index).asInstanceOf[Unit]
        index += 1
      }
      array2
    })

  def this(size: Int) =
    this(new Array[Unit](size))

  def apply(idx: Int): T = array(idx).asInstanceOf[T]
  def update(idx: Int, value: T): Unit = array(idx) = value.asInstanceOf[Unit]
  override def clone: MbArray[T] = new MbArray_V[T](array.clone())
  def length: Int = array.length

  // optimized accessors:
  def apply_J(idx: Int): Long = 0 // TODO: WTF
  def update_J(idx: Int, value: Long): Unit = array(idx) = Unit // TODO: WTF
  override def arraycopy(srcPos: Int, dest: MbArray[T], destPos: Int, length: Int) = {
    dest match {
      case dest_J: MbArray_V[T] => System.arraycopy(array, srcPos, dest_J.array, destPos, length)
      case _ => super.arraycopy(srcPos, dest, destPos, length)
    }
  }
}

class MbArray_J[T](private[array] final val array: Array[Long]) extends MbArray[T] {

  def this(array: Array[T]) =
    this({
      val array2 = new Array[Long](array.length)
      var index = 0
      while (index < array.length) {
        array2(index) = array(index).asInstanceOf[Long]
        index += 1
      }
      array2
    })

  def this(size: Int) =
    this(new Array[Long](size))

  def apply(idx: Int): T = array(idx).asInstanceOf[T]
  def update(idx: Int, value: T): Unit = array(idx) = value.asInstanceOf[Long]
  override def clone: MbArray[T] = new MbArray_J[T](array.clone())
  def length: Int = array.length

  // optimized accessors:
  def apply_J(idx: Int): Long = MiniboxConversions.long2minibox(array(idx))
  def update_J(idx: Int, value: Long): Unit = array(idx) = MiniboxConversions.minibox2long(value)
  override def arraycopy(srcPos: Int, dest: MbArray[T], destPos: Int, length: Int) = {
    dest match {
      case dest_J: MbArray_J[T] => System.arraycopy(array, srcPos, dest_J.array, destPos, length)
      case _ => super.arraycopy(srcPos, dest, destPos, length)
    }
  }
}


class MbArray_D[T](T_Tag: Byte, private[array] final val array: Array[Double]) extends MbArray[T] {

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

  def this(T_Tag: Byte, size: Int) =
    this(T_Tag, new Array[Double](size))

  def apply(idx: Int): T = MiniboxConversionsDouble.minibox2box(array(idx), T_Tag)
  def update(idx: Int, value: T): Unit = array(idx) = MiniboxConversionsDouble.box2minibox_tt(value, T_Tag)
  override def clone: MbArray[T] = new MbArray_D[T](T_Tag, array.clone())
  def length: Int = array.length

  // optimized accessors:
  def apply_D(idx: Int): Double = array(idx)
  def update_D(idx: Int, value: Double): Unit = array(idx) = value
  override def arraycopy(srcPos: Int, dest: MbArray[T], destPos: Int, length: Int) = {
    dest match {
      case dest_D: MbArray_D[T] => System.arraycopy(array, srcPos, dest_D.array, destPos, length)
      case _ => super.arraycopy(srcPos, dest, destPos, length)
    }
  }
}