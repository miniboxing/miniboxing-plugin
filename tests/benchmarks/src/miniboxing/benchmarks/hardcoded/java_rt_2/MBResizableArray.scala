package miniboxing.benchmarks.hardcoded.java_rt_2

import miniboxing.runtime.alternative.java_runtime.MiniboxArray2._
import miniboxing.runtime.MiniboxConstants._
import miniboxing.runtime.MiniboxConversions._
import miniboxing.runtime.MiniboxDispatch._
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

  def array: Array[T]                           // getter
  def array_=(newArray: Array[T]): Unit         // setter
  def array_J: Array[T]                         // getter (specialized)
  def array_J_=(newArray: Array[T]): Unit       // setter (specialized)

  def newarray: Array[T]                        // getter
  def newarray_=(newNewArray: Array[T]): Unit   // setter
  def newarray_J: Array[T]                      // getter (specialized)
  def newarray_J_=(newNewArray: Array[T]): Unit // setter (specialized)

  // methods
  // extend
  def extend(): Unit
  def extend_J(): Unit

  // add
  def add(elem: T)
  def add_J(elem: Long, T_Type_m: Byte)

  // reverse
  def reverse(): Unit
  def reverse_J(): Unit

  // contains
  def contains(elem: T): Boolean
  def contains_J(elem: Long, T_Type_m: Byte): Boolean

  // length
  def length: Int
  def length_J: Int

  // setElement
  def setElement(p: Int, t: T): Unit
  def setElement_J(p: Int, t: Long, T_Type_m: Byte): Unit

  // getElement
  def getElement(p: Int): T
  def getElement_J(p: Int, T_Type_m: Byte): Long

  // toString
  def toString(): String
  def toString_J(): String
}

class MBResizableArray_class_J[Tsp: Manifest](T_TypeTag: Byte) extends MBResizableArray[Tsp] {
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

  private[this] var _array_J: Array[Tsp] = mbarray_new(initialSize, T_TypeTag).asInstanceOf[Array[Tsp]]
  def array: Array[Tsp] = array_J
  def array_=(newArray: Array[Tsp]): Unit = array_J_=(newArray)
  def array_J: Array[Tsp] = _array_J
  def array_J_=(newArray: Array[Tsp]): Unit = _array_J = newArray

  private[this] var _newarray_J: Array[Tsp] = null
  def newarray: Array[Tsp] = _newarray_J
  def newarray_=(newNewArray: Array[Tsp]): Unit = newarray_J_=(newNewArray)
  def newarray_J: Array[Tsp] = _newarray_J
  def newarray_J_=(newNewArray: Array[Tsp]): Unit = _newarray_J = newNewArray


  // methods
  // extend
  def extend(): Unit = extend_J
  def extend_J(): Unit = {
    if (mboxed_eqeq(elemCount_J, size_J)) {
      var pos = 0
      newarray_J = mbarray_new(2 * size, T_TypeTag).asInstanceOf[Array[Tsp]]// new Array[Tsp](2 * size)
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
  def add(elem: Tsp) = add_J(box2minibox(elem), T_TypeTag)
  def add_J(elem: Long, T_Type_m: Byte) = {
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
      val tmp1: Long = getElement_J(pos, T_TypeTag)
      val tmp2: Long = getElement_J(elemCount_J-pos-1, T_TypeTag)
      setElement_J(pos, tmp2, T_TypeTag)
      setElement_J(elemCount_J-pos-1, tmp1, T_TypeTag)
      pos += 1
    }
  }


  // contains
  def contains(elem: Tsp): Boolean = contains_J(box2minibox(elem), T_TypeTag)
  def contains_J(elem: Long, T_Type_m: Byte): Boolean = {
    var pos = 0
    while (pos < elemCount_J){
      if (mboxed_eqeq(getElement_J(pos, T_TypeTag), elem))
        return true
      pos += 1
    }
    return false
  }

  // length
  def length: Int = length_J
  def length_J: Int = elemCount_J

  // setElement
  def setElement(p: Int, t: Tsp): Unit = setElement_J(p, box2minibox(t), T_TypeTag)
  def setElement_J(p: Int, t: Long, T_Type_m: Byte) = {
    //array.asInstanceOf[Array[Tsp]](p) = t
    mbarray_update_minibox(array_J, p, t, T_TypeTag)
  }

  // getElement
  def getElement(p: Int): Tsp = minibox2box(getElement_J(p, T_TypeTag), T_TypeTag)
  def getElement_J(p: Int, T_Type_m: Byte): Long = {
    // array.asInstanceOf[Array[Tsp]](p)
    mbarray_apply_minibox(array_J, p, T_TypeTag)
  }

  // toString
  override def toString(): String = toString_J()
  def toString_J(): String = {
    var pos = 0
    var ret = ""
    while (pos < elemCount_J) {
      ret += mboxed_toString(getElement_J(pos, T_TypeTag), T_TypeTag) + ", "
      pos  += 1
    }
    ret
  }
}

class MBResizableArray_L[Tsp: Manifest]() extends MBResizableArray[Tsp] {
  // initialSize
  private[this] final val _initialSize_L = 4
  def initialSize: Int = _initialSize_L
  def initialSize_J: Int = initialSize

  // size
  private[this] var _size_L: Int = initialSize
  def size: Int = _size_L
  def size_=(newSize: Int): Unit = _size_L = newSize
  def size_J: Int = size
  def size_J_=(newSize: Int): Unit = size_=(newSize)

  private[this] var _elemCount_L: Int = 0
  def elemCount: Int = _elemCount_L
  def elemCount_=(newElemCount: Int): Unit = _elemCount_L = newElemCount
  def elemCount_J: Int = elemCount
  def elemCount_J_=(newElemCount: Int): Unit = elemCount_=(newElemCount)

  private[this] var _array_L: Array[Tsp] = new Array[Tsp](initialSize)
  def array: Array[Tsp] = _array_L
  def array_=(newArray: Array[Tsp]): Unit = _array_L = newArray
  def array_J: Array[Tsp] = array
  def array_J_=(newArray: Array[Tsp]): Unit = array_=(newArray)

  private[this] var _newarray_L: Array[Tsp] = null
  def newarray: Array[Tsp] = _newarray_L
  def newarray_=(newNewArray: Array[Tsp]): Unit = _newarray_L = newNewArray
  def newarray_J: Array[Tsp] = newarray
  def newarray_J_=(newNewArray: Array[Tsp]): Unit = newarray_=(newNewArray)


  // methods
  // extend
  def extend(): Unit = {
    if (elemCount == size) {
      var pos = 0
      newarray = new Array[Tsp](2 * size)
      while(pos < size) {
        newarray(pos) = array(pos)
        pos += 1
      }
      array = newarray
      size *= 2
    }
  }
  def extend_J(): Unit = extend()


  // add
  def add(elem: Tsp) = {
    extend()
    array(elemCount) = elem
    elemCount += 1
  }
  def add_J(elem: Long, T_Type_m: Byte) = add(minibox2box(elem, T_Type_m))



  // reverse
  def reverse(): Unit = {
    var pos = 0
    while (pos * 2 < elemCount) {
      val tmp1: Tsp = getElement(pos)
      val tmp2: Tsp = getElement(elemCount-pos-1)
      setElement(pos, tmp2)
      setElement(elemCount-pos-1, tmp1)
      pos += 1
    }
  }
  def reverse_J(): Unit = reverse()


  // contains
  def contains(elem: Tsp): Boolean = {
    var pos = 0
    while (pos < elemCount){
      if (getElement(pos) == elem)
        return true
      pos += 1
    }
    return false
  }
  def contains_J(elem: Long, T_Type_m: Byte): Boolean = contains(minibox2box(elem, T_Type_m))

  // length
  def length: Int = elemCount
  def length_J: Int = length

  // setElement
  def setElement(p: Int, t: Tsp): Unit = {
    array(p) = t
  }
  def setElement_J(p: Int, t: Long, T_Type_m: Byte) = setElement(p, minibox2box(t, T_Type_m))

  // getElement
  def getElement(p: Int): Tsp = {
    array(p)
  }

  def getElement_J(p: Int, T_Type_m: Byte): Long = box2minibox(getElement(p))

  // toString
  override def toString(): String = {
    var pos = 0
    var ret = ""
    while (pos < elemCount) {
      ret += getElement(pos).toString + ", "
      pos  += 1
    }
    ret
  }
  def toString_J(): String = toString()
}
