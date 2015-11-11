package miniboxing.benchmarks.hardcoded.semiswitch

import miniboxing.runtime.alternative.MiniboxArray_SemiSwitch._
import miniboxing.internal.MiniboxConstants._
import miniboxing.internal.MiniboxConversions._
import miniboxing.internal.MiniboxDispatch._
import scala.Int.int2long

/*
 * The arrays are tricky since we need to use the natural representation for them.
 *
 * The plugin if arrays are used inside minispeced code according to the following rules:
 *  - every array creation is done via: MiniboxArray.newArray[T](len)
 *  - every access to the array requires a cast to its type: array.asInstanceOf[Array[T]](p)
 *  - local array variables are not supported
 */
trait MBResizableArray[T] {
  // fields
  def initialSize: Int                          // getter
  def initialSize_J: Int                        // getter (specialized)

  def size: Int                                 // getter
  def size_=(newSize: Int): Unit                // setter
  def size_J: Int                               // getter (specialized)
  def size_J_=(newSize: Int): Unit              // setter (specialized)

  def elemCount: Int                            // getter
  def elemCount_=(newElemCount: Int): Unit      // setter
  def elemCount_J: Int                          // getter (specialized)
  def elemCount_J_=(newElemCount: Int): Unit    // setter (specialized)

  def array: Any                                // getter
  def array_=(newArray: Any): Unit              // setter
  def array_J: Any                              // getter (specialized)
  def array_J_=(newArray: Any): Unit            // setter (specialized)

  def newarray: Any                             // getter
  def newarray_=(newNewArray: Any): Unit        // setter
  def newarray_J: Any                           // getter (specialized)
  def newarray_J_=(newNewArray: Any): Unit      // setter (specialized)

  // methods
  // extend
  def extend(): Unit
  def extend_J(): Unit

  // add
  def add(elem: T)
  def add_J(elem: Long)

  // reverse
  def reverse(): Unit
  def reverse_J(): Unit

  // contains
  def contains(elem: T): Boolean
  def contains_J(elem: Long): Boolean

  // length
  def length: Int
  def length_J: Int

  // setElement
  def setElement(p: Int, t: T): Unit
  def setElement_J(p: Int, t: Long): Unit

  // getElement
  def getElement(p: Int): T
  def getElement_J(p: Int): Long

  // toString
  def toString(): String
  def toString_J(): String
}

class MBResizableArray_J[Tsp: Manifest](T_TypeTag: Byte) extends MBResizableArray[Tsp] {
  // initialSize
  private[this] final val _initialSize_J = 4
  def initialSize: Int = initialSize_J
  def initialSize_J: Int = _initialSize_J

  // size
  private[this] var _size_J: Int = initialSize_J
  def size: Int = size_J
  def size_=(newSize: Int): Unit = size_J_=(newSize)
  def size_J: Int = _size_J
  def size_J_=(newSize: Int): Unit = _size_J = newSize

  private[this] var _elemCount_J: Int = 0
  def elemCount: Int = elemCount_J
  def elemCount_=(newElemCount: Int): Unit = elemCount_J_=(newElemCount)
  def elemCount_J: Int = _elemCount_J
  def elemCount_J_=(newElemCount: Int): Unit = _elemCount_J = newElemCount

  private[this] var _array_J: Any = mbarray_new(initialSize, T_TypeTag)
  def array: Any = array_J
  def array_=(newArray: Any): Unit = array_J_=(newArray)
  def array_J: Any = _array_J
  def array_J_=(newArray: Any): Unit = _array_J = newArray

  private[this] var _newarray_J: Any = null
  def newarray: Any = _newarray_J
  def newarray_=(newNewArray: Any): Unit = newarray_J_=(newNewArray)
  def newarray_J: Any = _newarray_J
  def newarray_J_=(newNewArray: Any): Unit = _newarray_J = newNewArray


  // methods
  // extend
  def extend(): Unit = extend_J
  def extend_J(): Unit = {
    if (mboxed_eqeq(elemCount_J, size_J)) {
      var pos = 0
      newarray_J = mbarray_new(2 * size, T_TypeTag)// new Array[Tsp](2 * size)
      while(pos < size_J) {
        // newarray.asInstanceOf[Array[Tsp]](pos) = array.asInstanceOf[Array[Tsp]](pos)
        mbarray_update_minibox(newarray_J, pos, mbarray_apply_minibox(array_J, pos, T_TypeTag), T_TypeTag)
        pos += 1
      }
      array_J = newarray_J
      size_J *= 2
    }
  }


  // add
  def add(elem: Tsp) = add_J(box2minibox(elem))
  def add_J(elem: Long) = {
    extend_J()
    //array.asInstanceOf[Array[Tsp]](elemCount) = elem
    mbarray_update_minibox(array_J, elemCount_J, elem, T_TypeTag)
    elemCount_J += 1
  }



  // reverse
  def reverse(): Unit = reverse_J()
  def reverse_J(): Unit = {
    var pos = 0
    while (pos * 2 < elemCount_J) {
      val tmp1: Long = getElement_J(pos)
      val tmp2: Long = getElement_J(elemCount_J-pos-1)
      setElement_J(pos, tmp2)
      setElement_J(elemCount_J-pos-1, tmp1)
      pos += 1
    }
  }


  // contains
  def contains(elem: Tsp): Boolean = contains_J(box2minibox(elem))
  def contains_J(elem: Long): Boolean = {
    var pos = 0
    while (pos < elemCount_J){
      if (mboxed_eqeq(getElement_J(pos), elem))
        return true
      pos += 1
    }
    return false
  }

  // length
  def length: Int = length_J
  def length_J: Int = elemCount_J

  // setElement
  @inline final def setElement(p: Int, t: Tsp): Unit = setElement_J(p, box2minibox(t))
  @inline final def setElement_J(p: Int, t: Long) = {
    //array.asInstanceOf[Array[Tsp]](p) = t
    mbarray_update_minibox(array_J, p, t, T_TypeTag)
  }

  // getElement
  @inline final def getElement(p: Int): Tsp = minibox2box(getElement_J(p), T_TypeTag)
  @inline final def getElement_J(p: Int): Long = {
    // array.asInstanceOf[Array[Tsp]](p)
    mbarray_apply_minibox(array_J, p, T_TypeTag)
  }

  // toString
  override def toString(): String = toString_J()
  def toString_J(): String = {
    var pos = 0
    var ret = ""
    while (pos < elemCount_J) {
      ret += mboxed_toString(getElement_J(pos), T_TypeTag) + ", "
      pos  += 1
    }
    ret
  }
}
