package miniboxing.benchmarks.simple.specialized

// Function
// NOTE: We don't want specialization to generate 100 / 1000 classes here,
// but otherwise it simply crashes with an AbstractMethodError - the bytecode
// it generates is probably missing one or more methods
trait Function1[@specialized -T, @specialized +S] {
  def apply(t: T): S
}

trait Function2[@specialized -T1, @specialized -T2, @specialized +R] {
  def apply(t1: T1, t2: T2): R
}



// Tuple
case class Tuple2[@specialized +T1, @specialized +T2](_1: T1, _2: T2) {
  override def toString() = "(" + _1.toString + "," + _2.toString + ")"
}



// Builder
trait Builder[@specialized -T, +To] {

  def +=(e1: T): Unit
  def finalise: To
}

// Can build from implicit
trait CanBuildFrom[-From, -Elem, +To] {

  def apply(): Builder[Elem, To]
}


class ListBuilder[@specialized T] extends Builder[T, List[T]] {

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
trait Numeric[@specialized T] {
  def plus(x: T, y: T): T
  def zero: T
}



// Traversable
trait Traversable[@specialized +T] extends TraversableLike[T, Traversable[T]]

trait TraversableLike[@specialized +T, +Repr] {

  def mapTo[@specialized U, To](f: Function1[T, U])(b: Builder[U, To]): To = {
    foreach(new Function1[T,Unit] { def apply(t: T): Unit = b += f(t) })
    b.finalise
  }

  def map[@specialized U, That](f: Function1[T, U])(implicit cbf: CanBuildFrom[Repr, U, That]): That = mapTo[U, That](f)(cbf())

  def sum[@specialized B >: T](implicit n : Numeric[B]): B = foldLeft(n.zero) {
    new Function2[B, T, B] { def apply(b: B, t: T): B = n.plus(b, t) }
  }

  def foreach[@specialized U](f: Function1[T, U]): Unit

  def foldLeft[@specialized B](z: B)(op: Function2[B, T, B]): B
}



// Iterable
trait Iterable[@specialized +T] extends Traversable[T] {
  def iterator: Iterator[T]
}


trait IterableLike[+T, +Repr] extends Traversable[T] {

  def iterator: Iterator[T]

  def zipTo[@specialized B, To](that: Iterable[B])(b: Builder[Tuple2[T, B], To]): To = {
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      b += (new Tuple2(these.next, those.next))
    b.finalise
  }

  def zip[@specialized U, That](that: Iterable[U])(implicit cbf: CanBuildFrom[Repr, Tuple2[T, U], That]): That = zipTo[U, That](that)(cbf())
}


// Iterator
trait Iterator[@specialized +T] {
  def hasNext(): Boolean
  def next(): T
}


trait LinearSeqOptimized[@specialized +A] extends Iterable[A] {

  def isEmpty: Boolean

  def head: A

  def tail: LinearSeqOptimized[A]

  override /*IterableLike*/
  def foreach[@specialized B](f: Function1[A, B]) {
    var these = this
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }

  override /*TraversableLike*/
  def foldLeft[@specialized B](z: B)(f: Function2[B, A, B]): B = {
    var acc = z
    var these = this
    while (!these.isEmpty) {
      acc = f(acc, these.head)
      these = these.tail
    }
    acc
  }
}


// List
// NOTE: For specialization to work, List needs to be a trait instead of an abstract class
trait List[@specialized +T] extends Traversable[T] with TraversableLike[T, List[T]] with Iterable[T] with IterableLike[T, List[T]] with LinearSeqOptimized[T] {

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
  def foreach[@specialized U](f: Function1[T, U]) {
    var these = this
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }

  def head: T
  def tail: List[T]
  def size: Int

  def ::[@specialized S >: T](e1: S) : List[S] = new ::[S](e1, this)

  def reverse: List[T] = {
    val it = iterator
    var list: List[T] = Nil
    while (it.hasNext) list = it.next :: list
    list
  }
}

object List {
  implicit def canBuildFrom[@specialized A]: CanBuildFrom[List[_], A, List[A]] =
    new CanBuildFrom[List[_], A, List[A]] {
      def apply = new ListBuilder[A]
    }
}


case class ::[@specialized T](head: T, var tail: List[T]) extends List[T] {
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
