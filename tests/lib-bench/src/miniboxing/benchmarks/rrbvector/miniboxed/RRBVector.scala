package miniboxing.benchmarks.rrbvector.miniboxed

import scala.collection.generic._
import scala.annotation.unchecked.uncheckedVariance

object RRBVector {
    def newBuilder[@miniboxed A]: Builder[A, RRBVector[A]] = new RRBVectorBuilder[A]()

    @inline private[rrbvector] final val compileAssertions = false

    implicit def canBuildFrom[@miniboxed A]: CanBuildFrom[RRBVector[_], A, RRBVector[A]] =
      new CanBuildFrom[RRBVector[_], A, RRBVector[A]] {
        def apply = new RRBVectorBuilder[A]
      }

    lazy private val EMPTY_VECTOR = new RRBVector[Nothing](0)

    def empty[@miniboxed A]: RRBVector[A] = EMPTY_VECTOR

    private[rrbvector] final val emptyTransientBlock = new Array[AnyRef](2)
}

final class RRBVector[@miniboxed +A] private[rrbvector](override private[rrbvector] val endIndex: Int) extends Traversable[A] with TraversableLike[A, RRBVector[A]] with Iterable[A] with IterableLike[A, RRBVector[A]] with RRBVectorPointer[A@uncheckedVariance] with Serializable {
  self =>

    private[rrbvector] var transient: Boolean = false

    def length: Int = endIndex

    def lengthCompare(len: Int): Int = endIndex - len

    override def iterator: RRBVectorIterator[A] = {
        if (this.transient) {
            this.normalize(depth)
            this.transient = false
        }
        val it = new RRBVectorIterator[A](0, endIndex)
        it.initIteratorFrom(this)
        it
    }

    def reverseIterator: RRBVectorReverseIterator[A] = {
        if (this.transient) {
            this.normalize(depth)
            this.transient = false
        }
        val it = new RRBVectorReverseIterator[A](0, endIndex)
        it.initIteratorFrom(this)
        it
    }

    def reverse: RRBVector[A] = {
      val reverted = RRBVector.newBuilder[A]
      val iter = reverseIterator
      while (iter.hasNext) {
        val value = iter.next()
        reverted += value
      }
      reverted.result
    }

    def foreach[@miniboxed B](f: A => B): Unit = {
      val iter = iterator
      while (iter.hasNext) {
        val value = iter.next()
        f(value)
      }
    }


    def apply(index: Int): A = {
        // keep method size under 35 bytes, so that it can be JIT-inlined

        def getElemFromInsideFocus(index: Int, _focusStart: Int): A = {
            // extracted to keep method size under 35 bytes, so that it can be JIT-inlined
            val indexInFocus = index - _focusStart
            getElem(indexInFocus, indexInFocus ^ focus)
        }

        def getElemFromOutsideFocus(index: Int): A = {
            // extracted to keep method size under 35 bytes, so that it can be JIT-inlined
            if /* index is in the vector bounds */ (0 <= index && index < endIndex) {
                if (transient) {
                    normalize(depth)
                    transient = false
                }
                return getElementFromRoot(index)
            } else
                throw new IndexOutOfBoundsException(index.toString)
        }

        val _focusStart = focusStart
        if /* index is in focused subtree */ (_focusStart <= index && index < focusEnd) {
            return getElemFromInsideFocus(index, _focusStart)
        } else {
            return getElemFromOutsideFocus(index)
        }
    }
}

final class RRBVectorBuilder[@miniboxed A] extends Builder[A, RRBVector[A]] /*with RRBVectorPointer[A@uncheckedVariance] */ {
    private final var display0: MbArray[A] = MbArray.empty[A](32)
    private final var display1: Array[AnyRef] = _
    private final var display2: Array[AnyRef] = _
    private final var display3: Array[AnyRef] = _
    private final var display4: Array[AnyRef] = _
    private final var display5: Array[AnyRef] = _
    private final var depth = 1
    private final var blockIndex = 0
    private final var lo = 0

    def finalise: RRBVector[A] = result


    final def +=(elem: A): this.type = {
        def nextBlock() = {
            val _blockIndex = blockIndex
            val newBlockIndex = _blockIndex + 32
            blockIndex = newBlockIndex
            gotoNextBlockStartWritable(newBlockIndex ^ _blockIndex)
        }
        var _lo = lo
        if (_lo >= 32) {
            nextBlock()
            _lo = 0
        }
        display0(_lo) = elem
        lo = _lo + 1
        this
    }


    private final def resultCurrent(): RRBVector[A] = {
        val _lo = lo
        val size = blockIndex + _lo
        if (size == 0) {
            return RRBVector.empty
        } else {
            val resultVector = new RRBVector[A](size)
            var d0 = display0
            if (_lo != 32) {
                val d0_truncated = MbArray.empty[A](_lo)
                MbArray.arraycopy(d0, 0, d0_truncated, 0, _lo)
                d0 = d0_truncated
            }
            resultVector.focusEnd = size
            val _depth = depth
            resultVector.focusDepth = _depth
            val lastIndex = size - 1
            _depth match {
                case 1 =>
                    resultVector.initFromDisplays(d0)
                    return resultVector
                case 2 =>
                    def init() = {
                        val d1 = copyOfAndStabilize(display1, d0, (lastIndex >> 5) & 31)
                        resultVector.initFromDisplays(d1(0).asInstanceOf[MbArray[A]], d1)
                    }
                    init()
                    return resultVector
                case 3 =>
                    def init() = {
                        val d1 = copyOfAndStabilize(display1, d0, (lastIndex >> 5) & 31)
                        val d2 = copyOfAndStabilize(display2, d1, (lastIndex >> 10) & 31)
                        val d1_0 = d2(0).asInstanceOf[Array[AnyRef]]
                        val d0_0 = d1_0(0).asInstanceOf[MbArray[A]]
                        resultVector.initFromDisplays(d0_0, d1_0, d2)
                    }
                    init()
                    return resultVector
                case 4 =>
                    def init() = {
                        val d1 = copyOfAndStabilize(display1, d0, (lastIndex >> 5) & 31)
                        val d2 = copyOfAndStabilize(display2, d1, (lastIndex >> 10) & 31)
                        val d3 = copyOfAndStabilize(display3, d2, (lastIndex >> 15) & 31)
                        val d2_0 = d3(0).asInstanceOf[Array[AnyRef]]
                        val d1_0 = d2_0(0).asInstanceOf[Array[AnyRef]]
                        val d0_0 = d1_0(0).asInstanceOf[MbArray[A]]
                        resultVector.initFromDisplays(d0_0, d1_0, d2_0, d3)
                    }
                    init()
                    return resultVector
                case 5 =>
                    def init() = {
                        val d1 = copyOfAndStabilize(display1, d0, (lastIndex >> 5) & 31)
                        val d2 = copyOfAndStabilize(display2, d1, (lastIndex >> 10) & 31)
                        val d3 = copyOfAndStabilize(display3, d2, (lastIndex >> 15) & 31)
                        val d4 = copyOfAndStabilize(display4, d3, (lastIndex >> 20) & 31)
                        val d3_0 = d4(0).asInstanceOf[Array[AnyRef]]
                        val d2_0 = d3_0(0).asInstanceOf[Array[AnyRef]]
                        val d1_0 = d2_0(0).asInstanceOf[Array[AnyRef]]
                        val d0_0 = d1_0(0).asInstanceOf[MbArray[A]]
                        resultVector.initFromDisplays(d0_0, d1_0, d2_0, d3_0, d4)
                    }
                    init()
                    return resultVector
                case 6 =>
                    def init() = {
                        val d1 = copyOfAndStabilize(display1, d0, (lastIndex >> 5) & 31)
                        val d2 = copyOfAndStabilize(display2, d1, (lastIndex >> 10) & 31)
                        val d3 = copyOfAndStabilize(display3, d2, (lastIndex >> 15) & 31)
                        val d4 = copyOfAndStabilize(display4, d3, (lastIndex >> 20) & 31)
                        val d5 = copyOfAndStabilize(display5, d4, (lastIndex >> 25) & 31)
                        val d4_0 = d5(0).asInstanceOf[Array[AnyRef]]
                        val d3_0 = d4_0(0).asInstanceOf[Array[AnyRef]]
                        val d2_0 = d3_0(0).asInstanceOf[Array[AnyRef]]
                        val d1_0 = d2_0(0).asInstanceOf[Array[AnyRef]]
                        val d0_0 = d1_0(0).asInstanceOf[MbArray[A]]
                        resultVector.initFromDisplays(d0_0, d1_0, d2_0, d3_0, d4_0, d5)
                    }
                    init()
                    return resultVector
            }
        }
    }

    private final def copyOfAndStabilize(array: Array[AnyRef], lastChild: AnyRef, indexOfLastChild: Int) = {
        if (RRBVector.compileAssertions) {
            assert(array != null)
            assert(0 <= indexOfLastChild && indexOfLastChild < array.length, (indexOfLastChild, array.length))
        }
        val newArray = new Array[AnyRef](indexOfLastChild + 2)
        System.arraycopy(array, 0, newArray, 0, indexOfLastChild)
        newArray(indexOfLastChild) = lastChild
        newArray
    }

    private final def clearCurrent(): Unit = {
        display0 = MbArray.empty[A](32)
        display1 = null
        display2 = null
        display3 = null
        display4 = null
        display5 = null
        depth = 1
        blockIndex = 0
        lo = 0
    }

    final def clear(): Unit = {
        clearCurrent()
    }

    final def result(): RRBVector[A] = {
        val resultVector = resultCurrent()
        resultVector
    }

    private final def gotoNextBlockStartWritable(xor: Int): Unit = {
        if (xor < 1024) {
            def gotoNextBlockStartWritable() = {
                val d1: Array[AnyRef] =
                    if (depth == 1) {
                        depth = 2
                        val d1 = new Array[AnyRef](33)
                        d1(0) = display0
                        display1 = d1
                        d1
                    } else {
                        display1
                    }
                val d0 = MbArray.empty[A](32)
                display0 = d0
                d1((blockIndex >> 5) & 31) = d0
            }
            gotoNextBlockStartWritable()
            return
        } else if (xor < 32768) {
            def gotoNextBlockStartWritable() = {
                val d2: Array[AnyRef] =
                    if (depth == 2) {
                        depth = 3
                        val d2 = new Array[AnyRef](33)
                        d2(0) = display1
                        display2 = d2
                        d2
                    } else display2
                val d0 = MbArray.empty[A](32)
                val d1 = new Array[AnyRef](33)
                display0 = d0
                display1 = d1
                val index = blockIndex
                d1((index >> 5) & 31) = d0
                d2((index >> 10) & 31) = d1
            }
            gotoNextBlockStartWritable()
            return
        } else if (xor < 1048576) {
            def gotoNextBlockStartWritable() = {
                val d3: Array[AnyRef] =
                    if (depth == 3) {
                        depth = 4
                        val d3 = new Array[AnyRef](33)
                        d3(0) = display2
                        display3 = d3
                        d3
                    } else display3
                val d0 = MbArray.empty[A](32)
                val d1 = new Array[AnyRef](33)
                val d2 = new Array[AnyRef](33)
                display0 = d0
                display1 = d1
                display2 = d2
                val index = blockIndex
                d1((index >> 5) & 31) = d0
                d2((index >> 10) & 31) = d1
                d3((index >> 15) & 31) = d2
            }
            gotoNextBlockStartWritable()
            return
        } else if (xor < 33554432) {
            def gotoNextBlockStartWritable() = {
                val d4: Array[AnyRef] =
                    if (depth == 4) {
                        depth = 5
                        val d4 = new Array[AnyRef](33)
                        d4(0) = display3
                        display4 = d4
                        d4
                    } else display4
                val d0 = MbArray.empty[A](32)
                val d1 = new Array[AnyRef](33)
                val d2 = new Array[AnyRef](33)
                val d3 = new Array[AnyRef](33)
                display0 = d0
                display1 = d1
                display2 = d2
                display3 = d3
                val index = blockIndex
                d1((index >> 5) & 31) = d0
                d2((index >> 10) & 31) = d1
                d3((index >> 15) & 31) = d2
                d4((index >> 20) & 31) = d3
            }
            gotoNextBlockStartWritable()
            return
        } else if (xor < 1073741824) {
            def gotoNextBlockStartWritable() = {
                val d5: Array[AnyRef] =
                    if (depth == 5) {
                        depth = 6
                        val d5 = new Array[AnyRef](33)
                        d5(0) = display4
                        display5 = d5
                        d5
                    } else display5
                val d0 = MbArray.empty[A](32)
                val d1 = new Array[AnyRef](33)
                val d2 = new Array[AnyRef](33)
                val d3 = new Array[AnyRef](33)
                val d4 = new Array[AnyRef](33)
                display0 = d0
                display1 = d1
                display2 = d2
                display3 = d3
                display4 = d4
                val index = blockIndex
                d1((index >> 5) & 31) = d0
                d2((index >> 10) & 31) = d1
                d3((index >> 15) & 31) = d2
                d4((index >> 20) & 31) = d3
                d5((index >> 25) & 31) = d4
            }
            gotoNextBlockStartWritable()
            return
        } else
            throw new IllegalArgumentException()
    }
}

class RRBVectorIterator[@miniboxed +A](startIndex: Int, override private[rrbvector] val endIndex: Int) extends Iterator[A] with RRBVectorPointer[A@uncheckedVariance] {
    /* Index in the vector of the first element of current block, i.e. current display0 */
    private final var blockIndex: Int = _
    /* Index in current block, i.e. current display0 */
    private final var lo: Int = _
    /* End index (or length) of current block, i.e. current display0 */
    private final var endLo: Int = _
    private final var _hasNext: Boolean = _

    private[rrbvector] final def initIteratorFrom[@miniboxed B >: A](that: RRBVectorPointer[B]): Unit = {
        initWithFocusFrom(that.asInstanceOf[RRBVectorPointer[A]])
        _hasNext = startIndex < endIndex
        if (_hasNext) {
            focusOn(startIndex)
            blockIndex = focusStart + (focus & -32)
            lo = focus & 31
            if (endIndex < focusEnd)
                focusEnd = endIndex
            endLo = java.lang.Math.min(focusEnd - blockIndex, 32)
            return
        } else {
            blockIndex = 0
            lo = 0
            endLo = 1
            display0 = MbArray.empty[A](1)
        }
    }

    final def hasNext = _hasNext

    final def next(): A = {
        // keep method size under 35 bytes, so that it can be JIT-inlined
        var _lo = lo
        val res: A = display0(_lo)
        _lo += 1
        lo = _lo
        if (_lo == endLo)
            gotoNextBlock()
        res
    }

    private[rrbvector] final def gotoNextBlock(): Unit = {
        if (RRBVector.compileAssertions) {
            assert(lo == endLo)
        }
        val oldBlockIndex = blockIndex
        val newBlockIndex = oldBlockIndex + endLo
        blockIndex = newBlockIndex
        lo = 0
        val _focusEnd = focusEnd
        if (newBlockIndex < _focusEnd) {
            val _focusStart = focusStart
            val newBlockIndexInFocus = newBlockIndex - _focusStart
            gotoNextBlockStart(newBlockIndexInFocus, newBlockIndexInFocus ^ (oldBlockIndex - _focusStart))
            endLo = java.lang.Math.min(_focusEnd - newBlockIndex, 32)
            return
        } else {
            val _endIndex = endIndex
            if (newBlockIndex < _endIndex) {
                focusOn(newBlockIndex)
                if (_endIndex < focusEnd)
                    focusEnd = _endIndex
                endLo = java.lang.Math.min(focusEnd - newBlockIndex, 32)
                return
            } else {
                /* setup dummy index that will not fail with IndexOutOfBound in subsequent 'next()' invocations */
                lo = 0
                blockIndex = _endIndex
                endLo = 1
                if (_hasNext) {
                    _hasNext = false
                    return
                }
                else throw new NoSuchElementException("reached iterator end")
            }
        }
    }

    private[rrbvector] def remaining: Int = java.lang.Math.max(endIndex - (blockIndex + lo), 0)

}

class RRBVectorReverseIterator[@miniboxed +A](startIndex: Int, final override private[rrbvector] val endIndex: Int) extends Iterator[A] with RRBVectorPointer[A@uncheckedVariance] {
    private final var lastIndexOfBlock: Int = _
    private final var lo: Int = _
    private final var endLo: Int = _
    private final var _hasNext: Boolean = _

    private[rrbvector] final def initIteratorFrom[@miniboxed B >: A](that: RRBVectorPointer[B]): Unit = {
        initWithFocusFrom(that.asInstanceOf[RRBVectorPointer[A]])
        _hasNext = startIndex < endIndex
        if (_hasNext) {
            val idx = endIndex - 1
            focusOn(idx)
            lastIndexOfBlock = idx
            lo = (idx - focusStart) & 31
            endLo = java.lang.Math.max(startIndex - focusStart - lastIndexOfBlock, 0)
            return
        } else {
            lastIndexOfBlock = 0
            lo = 0
            endLo = 0
            display0 = MbArray.empty[A](1)
        }
    }

    final def hasNext = _hasNext

    final def next(): A = {
        // TODO push the check of _hasNext and the throwing of the NoSuchElementException into gotoPrevBlock() like in the normal RRBVectorIterator
        if (_hasNext) {
            var _lo = lo
            val res = display0(_lo)
            _lo -= 1
            lo = _lo
            if (_lo < endLo)
                gotoPrevBlock()
            res
        } else
            throw new NoSuchElementException("reached iterator end")
    }

    private[rrbvector] final def gotoPrevBlock(): Unit = {
        val newBlockIndex = lastIndexOfBlock - 32
        if (focusStart <= newBlockIndex) {
            val _focusStart = focusStart
            val newBlockIndexInFocus = newBlockIndex - _focusStart
            gotoPrevBlockStart(newBlockIndexInFocus, newBlockIndexInFocus ^ (lastIndexOfBlock - _focusStart))
            lastIndexOfBlock = newBlockIndex
            lo = 31
            endLo = java.lang.Math.max(startIndex - focusStart - focus, 0)
            return
        } else if (startIndex < focusStart) {
            val newIndex = focusStart - 1
            focusOn(newIndex)
            lastIndexOfBlock = newIndex
            lo = (newIndex - focusStart) & 31
            endLo = java.lang.Math.max(startIndex - focusStart - lastIndexOfBlock, 0)
            return
        } else {
            _hasNext = false
        }
    }
}

private[rrbvector] trait RRBVectorPointer[@miniboxed A] {
    private[rrbvector] final var display0: MbArray[A] = _
    private[rrbvector] final var display1: Array[AnyRef] = _
    private[rrbvector] final var display2: Array[AnyRef] = _
    private[rrbvector] final var display3: Array[AnyRef] = _
    private[rrbvector] final var display4: Array[AnyRef] = _
    private[rrbvector] final var display5: Array[AnyRef] = _
    private[rrbvector] final var depth: Int = _
    private[rrbvector] final var focusStart: Int = 0
    private[rrbvector] final var focusEnd: Int = 0
    private[rrbvector] final var focusDepth: Int = 0
    private[rrbvector] final var focus: Int = 0
    private[rrbvector] final var focusRelax: Int = 0

    private[rrbvector] def endIndex: Int

    private[rrbvector] final def initWithFocusFrom(that: RRBVectorPointer[A]): Unit = {
        initFocus(that.focus, that.focusStart, that.focusEnd, that.focusDepth, that.focusRelax)
        initFrom(that)
    }

    private[rrbvector] final def initFocus[@miniboxed U](focus: Int, focusStart: Int, focusEnd: Int, focusDepth: Int, focusRelax: Int): Unit = {
        this.focus = focus
        this.focusStart = focusStart
        this.focusEnd = focusEnd
        this.focusDepth = focusDepth
        this.focusRelax = focusRelax
    }

    private[rrbvector] final def initFromDisplays[@miniboxed U](display0: MbArray[A]): Unit = {
        this.depth = 1
        this.display0 = display0
    }

    private[rrbvector] final def initFromDisplays[@miniboxed U](display0: MbArray[A], display1: Array[AnyRef]): Unit = {
        this.depth = 2
        this.display0 = display0
        this.display1 = display1
    }

    private[rrbvector] final def initFromDisplays[@miniboxed U](display0: MbArray[A], display1: Array[AnyRef], display2: Array[AnyRef]): Unit = {
        this.depth = 3
        this.display0 = display0
        this.display1 = display1
        this.display2 = display2
    }

    private[rrbvector] final def initFromDisplays[@miniboxed U](display0: MbArray[A], display1: Array[AnyRef], display2: Array[AnyRef], display3: Array[AnyRef]): Unit = {
        this.depth = 4
        this.display0 = display0
        this.display1 = display1
        this.display2 = display2
        this.display3 = display3
    }

    private[rrbvector] final def initFromDisplays[@miniboxed U](display0: MbArray[A], display1: Array[AnyRef], display2: Array[AnyRef], display3: Array[AnyRef], display4: Array[AnyRef]): Unit = {
        this.depth = 5
        this.display0 = display0
        this.display1 = display1
        this.display2 = display2
        this.display3 = display3
        this.display4 = display4
    }

    private[rrbvector] final def initFromDisplays[@miniboxed U](display0: MbArray[A], display1: Array[AnyRef], display2: Array[AnyRef], display3: Array[AnyRef], display4: Array[AnyRef], display5: Array[AnyRef]): Unit = {
        this.depth = 6
        this.display0 = display0
        this.display1 = display1
        this.display1 = display2
        this.display3 = display3
        this.display4 = display4
        this.display5 = display5
    }


    private[rrbvector] final def initFrom(that: RRBVectorPointer[A]): Unit = {
        if (RRBVector.compileAssertions) {
            assert(that != null)
        }
        depth = that.depth
        that.depth match {
            case 0 =>
                return
            case 1 =>
                this.display0 = that.display0
                return
            case 2 =>
                this.display0 = that.display0
                this.display1 = that.display1
                return
            case 3 =>
                this.display0 = that.display0
                this.display1 = that.display1
                this.display2 = that.display2
                return
            case 4 =>
                this.display0 = that.display0
                this.display1 = that.display1
                this.display2 = that.display2
                this.display3 = that.display3
                return
            case 5 =>
                this.display0 = that.display0
                this.display1 = that.display1
                this.display2 = that.display2
                this.display3 = that.display3
                this.display4 = that.display4
                return
            case 6 =>
                this.display0 = that.display0
                this.display1 = that.display1
                this.display2 = that.display2
                this.display3 = that.display3
                this.display4 = that.display4
                this.display5 = that.display5
                return
            case _ => throw new IllegalStateException()
        }
    }

    private[rrbvector] final def initSingleton(elem: A): Unit = {
        initFocus(0, 0, 1, 1, 0)
        val d0 = MbArray.empty[A](1)
        d0(0) = elem
        display0 = d0
        depth = 1
    }

    private[rrbvector] final def root(): AnyRef = depth match {
        case 0 => return null
        case 1 => return display0
        case 2 => return display1
        case 3 => return display2
        case 4 => return display3
        case 5 => return display4
        case 6 => return display5
        case _ => throw new IllegalStateException()
    }

    private[rrbvector] final def focusOn(index: Int): Unit = {
        val _focusStart = focusStart
        if (_focusStart <= index && index < focusEnd) {
            val indexInFocus = index - _focusStart
            val xor = indexInFocus ^ focus
            if (xor >= 32)
                gotoPos(indexInFocus, xor)
            focus = indexInFocus
            return
        } else {
            gotoPosFromRoot(index)
        }
    }

    private[rrbvector] final def getElementFromRoot(index: Int): A = {
        if (RRBVector.compileAssertions) {
            assert(0 <= index)
            assert(1 < depth && depth <= 6, depth.toString)
        }
        var indexInSubTree = index
        var currentDepth = depth
        var display: Array[AnyRef] = currentDepth match {
            case 2 => display1
            case 3 => display2
            case 4 => display3
            case 5 => display4
            case 6 => display5
        }

        var sizes = display(display.length - 1).asInstanceOf[MbArray[Int]]
        do {
            val sizesIdx = getIndexInSizes(sizes, indexInSubTree)
            if (sizesIdx != 0)
                indexInSubTree -= sizes(sizesIdx - 1)
            display = display(sizesIdx).asInstanceOf[Array[AnyRef]]
            if (currentDepth == 2) {
                return getElem0(display(sizesIdx).asInstanceOf[MbArray[A]], indexInSubTree)
            }
            display = display(sizesIdx).asInstanceOf[Array[AnyRef]]
            if (currentDepth > 2)
                sizes = display(display.length - 1).asInstanceOf[MbArray[Int]]
            else
                sizes = null
            currentDepth -= 1
        } while (sizes != null)

        currentDepth match {
            case 2 => return getElem1(display, indexInSubTree)
            case 3 => return getElem2(display, indexInSubTree)
            case 4 => return getElem3(display, indexInSubTree)
            case 5 => return getElem4(display, indexInSubTree)
            case 6 => return getElem5(display, indexInSubTree)
            case _ => throw new IllegalStateException
        }
    }

    @inline private final def getIndexInSizes(sizes: MbArray[Int], indexInSubTree: Int): Int = {
        if (RRBVector.compileAssertions) {
            assert(0 <= indexInSubTree && indexInSubTree < sizes(sizes.length - 1))
        }
        if (indexInSubTree == 0)
            return 0
        var is = 0
        while (sizes(is) <= indexInSubTree)
            is += 1
        is
    }

    private[rrbvector] final def gotoPosFromRoot(index: Int): Unit = {
        if (RRBVector.compileAssertions) {
            assert(0 <= index)
        }
        var _startIndex: Int = 0
        var _endIndex: Int = endIndex
        var currentDepth: Int = depth
        var _focusRelax: Int = 0
        var continue: Boolean = currentDepth > 1

        if (continue) {
            var display: Array[AnyRef] = currentDepth match {
                case 2 => display1
                case 3 => display2
                case 4 => display3
                case 5 => display4
                case 6 => display5
                case _ => throw new IllegalStateException()
            }
            do {
                val sizes = display(display.length - 1).asInstanceOf[MbArray[Int]]
                if (sizes == null) {
                    continue = false
                } else {
                    val is = getIndexInSizes(sizes, index - _startIndex)
                    display = display(is).asInstanceOf[Array[AnyRef]]
                    currentDepth match {
                        case 2 =>
                            display0 = display.asInstanceOf[MbArray[A]]
                            continue = false
                        case 3 => display1 = display
                        case 4 => display2 = display
                        case 5 => display3 = display
                        case 6 => display4 = display
                    }
                    if (is < sizes.length - 1)
                        _endIndex = _startIndex + sizes(is)

                    if (is != 0)
                        _startIndex += sizes(is - 1)

                    currentDepth -= 1
                    _focusRelax |= is << (5 * currentDepth)
                }
            } while (continue)
        }
        val indexInFocus = index - _startIndex
        gotoPos(indexInFocus, 1 << (5 * (currentDepth - 1)))
        initFocus(indexInFocus, _startIndex, _endIndex, currentDepth, _focusRelax)
    }

    private[rrbvector] final def makeTransientSizes(oldSizes: MbArray[Int], transientBranchIndex: Int): MbArray[Int] = {
        val newSizes = MbArray.empty[Int](oldSizes.length)
        var delta = oldSizes(transientBranchIndex)
        if (transientBranchIndex > 0) {
            delta -= oldSizes(transientBranchIndex - 1)
            if (!oldSizes.eq(newSizes))
                System.arraycopy(oldSizes, 0, newSizes, 0, transientBranchIndex)
        }
        var i = transientBranchIndex
        val len = newSizes.length
        while (i < len) {
            newSizes(i) = oldSizes(i) - delta
            i += 1
        }
        newSizes
    }

    private final def makeNewRoot0(node: Array[AnyRef]): Array[AnyRef] = {
        val newRoot = new Array[AnyRef](3)
        newRoot(0) = node
        val dLen = node.length
        val dSizes = node(dLen - 1)
        if (dSizes != null) {
            val newRootSizes = MbArray.empty[Int](2)
            val dSize = dSizes.asInstanceOf[MbArray[Int]](dLen - 2)
            newRootSizes(0) = dSize
            newRootSizes(1) = dSize
            newRoot(2) = newRootSizes
        }
        newRoot
    }

    private final def makeNewRoot1(node: Array[AnyRef], currentDepth: Int): Array[AnyRef] = {
        val dSize = treeSize(node, currentDepth - 1)
        val newRootSizes = MbArray.empty[Int](2)
        /* newRootSizes(0) = 0 */
        newRootSizes(1) = dSize
        val newRoot = new Array[AnyRef](3)
        newRoot(1) = node
        newRoot(2) = newRootSizes
        newRoot
    }



    private final def copyAndIncLeftRoot(node: Array[AnyRef], transient: Boolean, currentLevel: Int): Array[AnyRef] = {
        val len = node.length
        val newRoot = new Array[AnyRef](len + 1)
        System.arraycopy(node, 0, newRoot, 1, len - 1)

        val oldSizes = node(len - 1)
        val newSizes = MbArray.empty[Int](len)
        if (oldSizes != null) {
            if (transient) {
                System.arraycopy(oldSizes, 1, newSizes, 2, len - 2)
            } else {
                System.arraycopy(oldSizes, 0, newSizes, 1, len - 1)
            }
        } else {
            val subTreeSize = 1 << (5 * currentLevel)
            var acc = 0
            var i = 1
            while (i < len - 1) {
                acc += subTreeSize
                newSizes(i) = acc
                i += 1
            }
            newSizes(i) = acc + treeSize(node(node.length - 2).asInstanceOf[Array[AnyRef]], currentLevel)
        }
        newRoot(len) = newSizes
        newRoot
    }





    private[rrbvector] final def getElem(index: Int, xor: Int): A = {
        if (xor < 32) return getElem0(display0, index)
        else if (xor < 1024) return getElem1(display1, index)
        else if (xor < 32768) return getElem2(display2, index)
        else if (xor < 1048576) return getElem3(display3, index)
        else if (xor < 33554432) return getElem4(display4, index)
        else if (xor < 1073741824) return getElem5(display5, index)
        else throw new IllegalArgumentException(xor.toString)
    }

    private final def getElem0(display: MbArray[A], index: Int): A =
        display(index & 31).asInstanceOf[A]

    private final def getElem1(display: Array[AnyRef], index: Int): A =
        display((index >> 5) & 31).asInstanceOf[Array[AnyRef]](index & 31).asInstanceOf[A]

    private final def getElem2(display: Array[AnyRef], index: Int): A =
        display((index >> 10) & 31).asInstanceOf[Array[AnyRef]]((index >> 5) & 31).asInstanceOf[Array[AnyRef]](index & 31).asInstanceOf[A]

    private final def getElem3(display: Array[AnyRef], index: Int): A =
        display((index >> 15) & 31).asInstanceOf[Array[AnyRef]]((index >> 10) & 31).asInstanceOf[Array[AnyRef]]((index >> 5) & 31).asInstanceOf[Array[AnyRef]](index & 31).asInstanceOf[A]

    private final def getElem4(display: Array[AnyRef], index: Int): A =
        display((index >> 20) & 31).asInstanceOf[Array[AnyRef]]((index >> 15) & 31).asInstanceOf[Array[AnyRef]]((index >> 10) & 31).asInstanceOf[Array[AnyRef]]((index >> 5) & 31).asInstanceOf[Array[AnyRef]](index & 31).asInstanceOf[A]

    private final def getElem5(display: Array[AnyRef], index: Int): A =
        display((index >> 25) & 31).asInstanceOf[Array[AnyRef]]((index >> 20) & 31).asInstanceOf[Array[AnyRef]]((index >> 15) & 31).asInstanceOf[Array[AnyRef]]((index >> 10) & 31).asInstanceOf[Array[AnyRef]]((index >> 5) & 31).asInstanceOf[Array[AnyRef]](index & 31).asInstanceOf[A]

    private[rrbvector] final def gotoPos(index: Int, xor: Int): Unit = {
        if (xor < 32)
            return
        else if (xor < 1024) {
            display0 = display1((index >> 5) & 31).asInstanceOf[MbArray[A]]
            return
        } else if (xor < 32768) {
            val d1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
            display1 = d1
            display0 = d1((index >> 5) & 31).asInstanceOf[MbArray[A]]
            return
        } else if (xor < 1048576) {
            val d2 = display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
            display2 = d2
            val d1 = d2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
            display1 = d1
            display0 = d1((index >> 5) & 31).asInstanceOf[MbArray[A]]
            return
        } else if (xor < 33554432) {
            val d3 = display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
            display3 = d3
            val d2 = d3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
            display2 = d2
            val d1 = d2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
            display1 = d1
            display0 = d1((index >> 5) & 31).asInstanceOf[MbArray[A]]
            return
        } else if (xor < 1073741824) {
            val d4 = display5((index >> 25) & 31).asInstanceOf[Array[AnyRef]]
            display4 = d4
            val d3 = d4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
            display3 = d3
            val d2 = d3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
            display2 = d2
            val d1 = d2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
            display1 = d1
            display0 = d1((index >> 5) & 31).asInstanceOf[MbArray[A]]
            return
        } else
            throw new IllegalArgumentException()
    }

    private[rrbvector] final def gotoNextBlockStart(index: Int, xor: Int): Unit = {
        if (xor < 1024) {
            display0 = display1((index >> 5) & 31).asInstanceOf[MbArray[A]]
            return
        } else if (xor < 32768) {
            val d1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
            display1 = d1
            display0 = d1(0).asInstanceOf[MbArray[A]]
            return
        } else if (xor < 1048576) {
            val d2 = display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
            val d1 = d2(0).asInstanceOf[Array[AnyRef]]
            display0 = d1(0).asInstanceOf[MbArray[A]]
            display1 = d1
            display2 = d2
            return
        } else if (xor < 33554432) {
            val d3 = display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
            val d2 = d3(0).asInstanceOf[Array[AnyRef]]
            val d1 = d2(0).asInstanceOf[Array[AnyRef]]
            display0 = d1(0).asInstanceOf[MbArray[A]]
            display1 = d1
            display2 = d2
            display3 = d3
            return
        } else if (xor < 1073741824) {
            val d4 = display5((index >> 25) & 31).asInstanceOf[Array[AnyRef]]
            val d3 = d4(0).asInstanceOf[Array[AnyRef]]
            val d2 = d3(0).asInstanceOf[Array[AnyRef]]
            val d1 = d2(0).asInstanceOf[Array[AnyRef]]
            display4 = d4
            display3 = d3
            display2 = d2
            display1 = d1
            display0 = d1(0).asInstanceOf[MbArray[A]]
            return
        } else
            throw new IllegalArgumentException()
    }

    private[rrbvector] final def gotoPrevBlockStart(index: Int, xor: Int): Unit = {
        if (xor < 1024) {
            display0 = display1((index >> 5) & 31).asInstanceOf[MbArray[A]]
            return
        } else if (xor < 32768) {
            val d1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
            display1 = d1
            display0 = d1(31).asInstanceOf[MbArray[A]]
            return
        } else if (xor < 1048576) {
            val d2 = display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
            display2 = d2
            val d1 = d2(31).asInstanceOf[Array[AnyRef]]
            display1 = d1
            display0 = d1(31).asInstanceOf[MbArray[A]]
            return
        } else if (xor < 33554432) {
            val d3 = display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
            display3 = d3
            val d2 = d3(31).asInstanceOf[Array[AnyRef]]
            display2 = d2
            val d1 = d2(31).asInstanceOf[Array[AnyRef]]
            display1 = d1
            display0 = d1(31).asInstanceOf[MbArray[A]]
            return
        } else if (xor < 1073741824) {
            val d4 = display5((index >> 25) & 31).asInstanceOf[Array[AnyRef]]
            display4 = d4
            val d3 = d4(31).asInstanceOf[Array[AnyRef]]
            display3 = d3
            val d2 = d3(31).asInstanceOf[Array[AnyRef]]
            display2 = d2
            val d1 = d2(31).asInstanceOf[Array[AnyRef]]
            display1 = d1
            display0 = d1(31).asInstanceOf[MbArray[A]]
            return
        } else
            throw new IllegalArgumentException()
    }

    private[rrbvector] final def normalize(_depth: Int): Unit = {
        if (RRBVector.compileAssertions) {
            assert(_depth > 1)
        }
        val _focusDepth = focusDepth
        val stabilizationIndex = focus | focusRelax
        copyDisplaysAndStabilizeDisplayPath(_focusDepth, stabilizationIndex)

        var currentLevel = _focusDepth
        if (currentLevel < _depth) {
            var display = currentLevel match {
                case 1 => display1
                case 2 => display2
                case 3 => display3
                case 4 => display4
                case 5 => display5
            }
            do {
                val newDisplay = copyOf(display)
                val idx = (stabilizationIndex >> (5 * currentLevel)) & 31
                currentLevel match {
                    case 1 =>
                        newDisplay(idx) = display0
                        display1 = withRecomputeSizes(newDisplay, 2, idx)
                        display = display2
                    case 2 =>
                        newDisplay(idx) = display1
                        display2 = withRecomputeSizes(newDisplay, 3, idx)
                        display = display3
                    case 3 =>
                        newDisplay(idx) = display2
                        display3 = withRecomputeSizes(newDisplay, 4, idx)
                        display = display4
                    case 4 =>
                        newDisplay(idx) = display3
                        display4 = withRecomputeSizes(newDisplay, 5, idx)
                        display = display5
                    case 5 =>
                        newDisplay(idx) = display4
                        display5 = withRecomputeSizes(newDisplay, 6, idx)
                }
                currentLevel += 1
            } while (currentLevel < _depth)
        }
    }



    private final def copyDisplaysAndStabilizeDisplayPath(_depth: Int, _focus: Int): Unit = {
        _depth match {
            case 1 =>
                return
            case 2 =>
                val d1 = copyOf(display1)
                d1((_focus >> 5) & 31) = display0
                display1 = d1
                return
            case 3 =>
                val d1 = copyOf(display1)
                d1((_focus >> 5) & 31) = display0
                display1 = d1
                val d2 = copyOf(display2)
                d2((_focus >> 10) & 31) = d1
                display2 = d2
                return
            case 4 =>
                val d1 = copyOf(display1)
                d1((_focus >> 5) & 31) = display0
                display1 = d1
                val d2 = copyOf(display2)
                d2((_focus >> 10) & 31) = d1
                display2 = d2
                val d3 = copyOf(display3)
                d3((_focus >> 15) & 31) = d2
                display3 = d3
                return
            case 5 =>
                val d1 = copyOf(display1)
                d1((_focus >> 5) & 31) = display0
                display1 = d1
                val d2 = copyOf(display2)
                d2((_focus >> 10) & 31) = d1
                display2 = d2
                val d3 = copyOf(display3)
                d3((_focus >> 15) & 31) = d2
                display3 = d3
                val d4 = copyOf(display4)
                d4((_focus >> 20) & 31) = d3
                display4 = d4
                return
            case 6 =>
                val d1 = copyOf(display1)
                d1((_focus >> 5) & 31) = display0
                display1 = d1
                val d2 = copyOf(display2)
                d2((_focus >> 10) & 31) = d1
                display2 = d2
                val d3 = copyOf(display3)
                d3((_focus >> 15) & 31) = d2
                display3 = d3
                val d4 = copyOf(display4)
                d4((_focus >> 20) & 31) = d3
                display4 = d4
                val d5 = copyOf(display5)
                d5((_focus >> 25) & 31) = d4
                display5 = d5
                return
        }
    }


    private[rrbvector] final def stabilizeDisplayPath(_depth: Int, _focus: Int): Unit = {
        if (_depth <= 1)
            return
        else {
            val d1 = display1
            d1((_focus >> 5) & 31) = display0
            if (_depth <= 2)
                return
            else {
                val d2 = display2
                d2((_focus >> 10) & 31) = d1
                if (_depth <= 3)
                    return
                else {
                    val d3 = display3
                    d3((_focus >> 15) & 31) = d2
                    if (_depth <= 4)
                        return
                    else {
                        val d4 = display4
                        d4((_focus >> 20) & 31) = d3
                        if (_depth > 5) {
                            display5((_focus >> 25) & 31) = d4
                        }
                    }
                }
            }
        }
        //                _depth match {
        //                    case 1 =>
        //                        return
        //                    case 2 =>
        //                        display1((_focus>>5)&31) = display0
        //                        return
        //                    case 3 =>
        //                        display2((_focus>>10)&31) = display1
        //                        display1((_focus>>5)&31) = display0
        //                        return
        //                    case 4 =>
        //                        display3((_focus>>15)&31) = display2
        //                        display2((_focus>>10)&31) = display1
        //                        display1((_focus>>5)&31) = display0
        //                        return
        //                    case 5 =>
        //                        display4((_focus>>20)&31) = display3
        //                        display3((_focus>>15)&31) = display2
        //                        display2((_focus>>10)&31) = display1
        //                        display1((_focus>>5)&31) = display0
        //                        return
        //                    case 6 =>
        //                        display5((_focus>>25)&31) = display4
        //                        display4((_focus>>20)&31) = display3
        //                        display3((_focus>>15)&31) = display2
        //                        display2((_focus>>10)&31) = display1
        //                        display1((_focus>>5)&31) = display0
        //                        return
        //                }
    }





    private[rrbvector] final def copyOf(array: Array[AnyRef]) = {
        if (RRBVector.compileAssertions) {
            assert(array != null)
        }
        val len = array.length
        val newArray = new Array[AnyRef](len)
        System.arraycopy(array, 0, newArray, 0, len)
        newArray
    }

    protected final def withRecomputeSizes(node: Array[AnyRef], currentDepth: Int, branchToUpdate: Int): Array[AnyRef] = {
        if (RRBVector.compileAssertions) {
            assert(node != null)
            assert(currentDepth > 1, currentDepth.toString)
            assert(node.length > 1, node.length.toString)
        }
        val end = node.length - 1
        val oldSizes = node(end).asInstanceOf[MbArray[Int]]
        if (oldSizes != null) {
            val newSizes = MbArray.empty[Int](end)

            val delta = treeSize(node(branchToUpdate).asInstanceOf[Array[AnyRef]], currentDepth - 1)
            if (branchToUpdate > 0)
                System.arraycopy(oldSizes, 0, newSizes, 0, branchToUpdate)
            var i = branchToUpdate
            while (i < end) {
                newSizes(i) = oldSizes(i) + delta
                i += 1
            }
            if (notBalanced(node, newSizes, currentDepth, end))
                node(end) = newSizes
        }
        node
    }


    @inline private final def notBalanced(node: Array[AnyRef], sizes: MbArray[Int], currentDepth: Int, end: Int): Boolean = {
        if (RRBVector.compileAssertions) {
            assert(end > 0, s"$end")
        }
        (end == 1 || sizes(end - 2) != ((end - 1) << (5 * (currentDepth - 1)))) || (
          (currentDepth > 2) && {
              val last = node(end - 1).asInstanceOf[Array[AnyRef]]
              last(last.length - 1) != null
          }
          )
    }

    private final def treeSize(node: Array[AnyRef], currentDepth: Int): Int = {
        def treeSizeRec(node: Array[AnyRef], currentDepth: Int, acc: Int): Int = {
            if (currentDepth == 1) {
                return acc + node.length
            } else {
                val treeSizes = node(node.length - 1).asInstanceOf[MbArray[Int]]
                if (treeSizes != null)
                    return acc + treeSizes(treeSizes.length - 1)
                else {
                    val len = node.length
                    return treeSizeRec(node(len - 2).asInstanceOf[Array[AnyRef]], currentDepth - 1, acc + (len - 2) * (1 << (5 * (currentDepth - 1))))
                }
            }
        }
        treeSizeRec(node, currentDepth, 0)
    }

}

// from Scala lib
trait Builder[@miniboxed -Elem, +To]  {
  def +=(elem: Elem): this.type
  def result(): To
  def clear(): Unit
  def finalise: To
}

trait CanBuildFrom[@miniboxed -From, @miniboxed -Elem, @miniboxed +To] {
  def apply(): Builder[Elem, To]
}

// Traversable
trait Traversable[@miniboxed +T] extends TraversableLike[T, Traversable[T]]

trait TraversableLike[@miniboxed +T, @miniboxed +Repr] {

  def mapTo[@miniboxed U, @miniboxed To](f: T => U)(b: Builder[U, To]): To = {
    foreach(t => b += f(t))
    b.finalise
  }

  def map[@miniboxed U, @miniboxed That](f: T => U)(implicit cbf: CanBuildFrom[Repr, U, That]): That = mapTo[U, That](f)(cbf())

  def foreach[@miniboxed U](f: T => U): Unit

  def foldLeft[@miniboxed B](z: B)(op: (B, T) => B): B = {
    var result = z
    this foreach (x => result = op(result, x))
    result
  }

  def fold[@miniboxed A1 >: T](z: A1)(op: (A1, A1) => A1): A1 = foldLeft(z)(op)
}

// Iterator
trait Iterator[@miniboxed +T] {
  def hasNext(): Boolean
  def next(): T
}

// Iterable
trait Iterable[@miniboxed +T] extends Traversable[T] {
  def iterator: Iterator[T]
}

trait IterableLike[@miniboxed +T, @miniboxed +Repr] extends Traversable[T] {
  def iterator: Iterator[T]
}

