package miniboxing.benchmarks.simple.miniboxed

// Function
trait Function1[@miniboxed -T, @miniboxed +S] {
  def apply(t: T): S
}



// Tuple
case class Tuple2[@miniboxed +T1, @miniboxed +T2](_1: T1, _2: T2) {
  override def toString() = "(" + _1.toString + "," + _2.toString + ")"
}



// Builder
trait Builder[@miniboxed -T, +To] {

  def +=(e1: T): Unit
  def finalise: To
}

class ListBuilder[@miniboxed T] extends Builder[T, List[T]] {

  private var start: List[T] = Nil
  private var last0: List[T] = Nil
  private var len: Int = 0

  def += (x: T): Unit = {
    if (start.isEmpty) {
      last0 = new :: (x, Nil)
      start = last0
    } else {
      val last1 = last0.asInstanceOf[::[T]]
      last0 = new :: (x, Nil)
      last1.tail = last0
    }
    len += 1
  }

  def finalise: List[T] = start
}



// Numeric
trait Numeric[@miniboxed T] {
  def plus(x: T, y: T): T
  def zero: T
}



// Traversable
trait Traversable[@miniboxed +T] {

  def mapTo[@miniboxed U, To](f: Function1[T, U])(b: Builder[U, To]): To = {
    foreach(new Function1[T,Unit] { def apply(t: T): Unit = b += f(t) })
    b.finalise
  }

  def map[@miniboxed U](f: Function1[T, U]): List[U] = mapTo[U, List[U]](f)(new ListBuilder)

  def sum[@miniboxed B >: T](implicit n : Numeric[B]): B = foldLeft(n.zero) {
    new Function1[Tuple2[B, T], B] { def apply(b: Tuple2[B, T]): B = n.plus(b._1, b._2) }
  }

  def foreach[@miniboxed U](f: Function1[T, U]): Unit

  def foldLeft[@miniboxed B](z: B)(op: Function1[Tuple2[B, T], B]): B
}



// Iterable
trait Iterable[@miniboxed +T] extends Traversable[T] {
  def iterator: Iterator[T]

  def zipTo[@miniboxed B, To](that: Iterable[B])(b: Builder[Tuple2[T, B], To]): To = {
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      b += (new Tuple2(these.next, those.next))
    b.finalise
  }

  def zip[@miniboxed U](that: Iterable[U]): List[Tuple2[T, U]] = zipTo[U, List[Tuple2[T, U]]](that)(new ListBuilder[Tuple2[T, U]])
}



// Iterator
trait Iterator[@miniboxed +T] {
  def hasNext(): Boolean
  def next(): T
}


trait LinearSeqOptimized[@miniboxed +A] extends Iterable[A] {

  def isEmpty: Boolean

  def head: A

  def tail: LinearSeqOptimized[A]

  override /*IterableLike*/
  def foreach[@miniboxed B](f: Function1[A, B]) {
    var these = this
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }

  override /*TraversableLike*/
  def foldLeft[@miniboxed B](z: B)(f: Function1[Tuple2[B, A], B]): B = {
    var acc = z
    var these = this
    while (!these.isEmpty) {
      acc = f(new Tuple2(acc, these.head))
      these = these.tail
    }
    acc
  }
}


// List
abstract class List[@miniboxed +T] extends Traversable[T] with Iterable[T] with LinearSeqOptimized[T] {

  def iterator = new Iterator[T] {
    var current = List.this
    def hasNext = current != Nil
    def next() = {
      val t = current.head
      current = current.tail
      t
    }
  }

  def isEmpty: Boolean

  @inline override final
  def foreach[@miniboxed U](f: Function1[T, U]) {
    var these = this
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }

  def head: T
  def tail: List[T]
  def size: Int

  def ::[@miniboxed S >: T](e1: S) : List[S] = new ::[S](e1, this)

  def reverse: List[T] = {
    val it = iterator
    var list: List[T] = Nil
    while (it.hasNext) list = it.next :: list
    list
  }
}

case class ::[@miniboxed T](head: T, var tail: List[T]) extends List[T] {
  def size = 1 + tail.size

  def isEmpty: Boolean = false

  override def toString = head.toString + " :: " + tail.toString
}

case object Nil extends List[Nothing] {
  def head = throw new NoSuchElementException("head of empty list")
  def tail = throw new NoSuchElementException("tail of empty list")

  def size = 0

  def isEmpty: Boolean = true

  override def toString = "Nil"
}

