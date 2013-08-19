package miniboxing.benchmarks.dispatcher

import miniboxing.runtime.alternative.Dispatcher

trait DispResizableArray[T] {
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
  def add(elem: T): Unit
  def add_J(elem: Long, disp_m: Dispatcher[T]): Unit

  // reverse
  def reverse(): Unit
  def reverse_J(): Unit

  // contains
  def contains(elem: T): Boolean
  def contains_J(elem: Long, disp_m: Dispatcher[T]): Boolean

  // length
  def length: Int
  def length_J: Int

  // setElement
  def setElement(p: Int, t: T): Unit
  def setElement_J(p: Int, t: Long, disp_m: Dispatcher[T]): Unit

  // getElement
  def getElement(p: Int): T
  def getElement_J(p: Int, disp_m: Dispatcher[T]): Long

  // toString
  def toString(): String
  def toString_J(): String
}

class DispResizableArray_class_J[Tsp: Manifest](disp: Dispatcher[Tsp]) extends DispResizableArray[Tsp] {
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

  private[this] var _array_J: Array[Tsp] = disp.mbarray_new(initialSize)
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
    if (disp.mboxed_eqeq(elemCount_J, size_J)) {
      var pos = 0
      newarray_J = disp.mbarray_new(2 * size)// new Array[Tsp](2 * size)
      while(pos < size_J) {
        // newarray.asInstanceOf[Array[Tsp]](pos) = array.asInstanceOf[Array[Tsp]](pos)
        disp.mbarray_update_minibox(newarray_J, pos, disp.mbarray_apply_minibox(array_J, pos))
        pos += 1
      }
      array_J = newarray_J
      size_J *= 2
    }
  }


  // add
  def add(elem: Tsp) = add_J(disp.box2minibox(elem), disp)
  def add_J(elem: Long, disp_m: Dispatcher[Tsp]) = {
    extend_J()
    //array.asInstanceOf[Array[Tsp]](elemCount) = elem
    disp.mbarray_update_minibox(array_J, elemCount_J, elem)
    elemCount += 1
  }



  // reverse
  def reverse(): Unit = reverse_J()
  def reverse_J(): Unit = {
    var pos = 0
    while (pos * 2 < elemCount_J) {
      val tmp1: Long = getElement_J(pos, disp)
      val tmp2: Long = getElement_J(elemCount_J-pos-1, disp)
      setElement_J(pos, tmp2, disp)
      setElement_J(elemCount_J-pos-1, tmp1, disp)
      pos += 1
    }
  }


  // contains
  def contains(elem: Tsp): Boolean = contains_J(disp.box2minibox(elem), disp)
  def contains_J(elem: Long, disp_m: Dispatcher[Tsp]): Boolean = {
    var pos = 0
    while (pos < elemCount_J){
      if (disp.mboxed_eqeq(getElement_J(pos, disp), elem))
        return true
      pos += 1
    }
    return false
  }

  // length
  def length: Int = length_J
  def length_J: Int = elemCount_J

  // setElement
  @inline final def setElement(p: Int, t: Tsp): Unit = setElement_J(p, disp.box2minibox(t), disp)
  @inline final def setElement_J(p: Int, t: Long, disp_m: Dispatcher[Tsp]) = {
    //array.asInstanceOf[Array[Tsp]](p) = t
    disp.mbarray_update_minibox(array_J, p, t)
  }

  // getElement
  @inline final def getElement(p: Int): Tsp = disp.minibox2box(getElement_J(p, disp))
  @inline final def getElement_J(p: Int, disp_m: Dispatcher[Tsp]): Long = {
    // array.asInstanceOf[Array[Tsp]](p)
    disp.mbarray_apply_minibox(array, p)
  }

  // toString
  override def toString(): String = toString_J()
  def toString_J(): String = {
    var pos = 0
    var ret = ""
    while (pos < elemCount_J) {
      ret += disp.mboxed_toString(getElement_J(pos, disp)) + ", "
      pos  += 1
    }
    ret
  }
}

class DispResizableArray_class_L[Tsp: Manifest]() extends DispResizableArray[Tsp] {
  // initialSize
  private[this] final val _initialSize_L = 4
  def initialSize: Int = _initialSize_L
  def initialSize_J: Int = initialSize

  // size
  private[this] var _size_L: Int = initialSize_J
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
      while(pos < size_J) {
        newarray(pos) = array(pos)
        pos += 1
      }
      array_J = newarray_J
      size_J *= 2
    }
  }
  def extend_J(): Unit = extend


  // add
  def add(elem: Tsp) = {
    extend_J()
    array(elemCount) = elem
    elemCount += 1
  }
  def add_J(elem: Long, disp_m: Dispatcher[Tsp]) = add(disp_m.minibox2box(elem))



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
  def contains_J(elem: Long, disp_m: Dispatcher[Tsp]): Boolean = contains(disp_m.minibox2box(elem))

  // length
  def length: Int = elemCount
  def length_J: Int = length

  // setElement
  @inline final def setElement(p: Int, t: Tsp): Unit = {
    array(p) = t
  }
  @inline final def setElement_J(p: Int, t: Long, disp_m: Dispatcher[Tsp]) = setElement(p, disp_m.minibox2box(t))

  // getElement
  @inline final def getElement(p: Int): Tsp = {
    array(p)
  }
  @inline final def getElement_J(p: Int, disp_m: Dispatcher[Tsp]): Long = disp_m.box2minibox(getElement(p))

  // toString
  override def toString(): String = {
    var pos = 0
    var ret = ""
    while (pos < elemCount_J) {
      ret += getElement(pos).toString + ", "
      pos  += 1
    }
    ret
  }
  def toString_J(): String = toString()
}


/*
 * That's a lot of bytecode for constructing the class.
 * TODO: Can we factor out some common functionalty?
 */
abstract class DispResizableArrayFactoryInterface {
  def newDispResizableArray_J[T$inst: Manifest](disp: Dispatcher[T$inst]): DispResizableArray[T$inst]
}

class DispResizableArrayFactoryInstance_class_J extends DispResizableArrayFactoryInterface {
  def newDispResizableArray_J[T$inst: Manifest](disp: Dispatcher[T$inst]): DispResizableArray[T$inst] =
    new DispResizableArray_class_J(disp)
}

object DispResizableArrayFactory {
  val fact = new Array[DispResizableArrayFactoryInterface](10)

  @inline def newDispResizableArray_J[T$inst: Manifest](disp: Dispatcher[T$inst]): DispResizableArray[T$inst] =
    if (fact(disp.tag) == null)
      createFactoryAndObject(disp.tag)(disp)
    else
      fact(disp.tag).newDispResizableArray_J(disp)

  def createFactoryAndObject[T$inst: Manifest](tag: Int)(disp: Dispatcher[T$inst]): DispResizableArray[T$inst] =
    try {
      val classloader = miniboxing.classloader.MiniboxingClassLoader.classloader(DispResizableArrayFactory.this)
      val clazz = classloader.findClass("miniboxing.benchmarks.dispatcher.DispResizableArrayFactoryInstance_class_" + tag)
      val inst  = clazz.newInstance().asInstanceOf[DispResizableArrayFactoryInterface]
      fact(tag) = inst
      fact(tag).newDispResizableArray_J(disp)
    } catch {
      // TODO: What exactly do we want to catch?
      case other: Throwable =>
        fact(tag) = new DispResizableArrayFactoryInstance_class_J()
        fact(tag).newDispResizableArray_J(disp)
    }
}
