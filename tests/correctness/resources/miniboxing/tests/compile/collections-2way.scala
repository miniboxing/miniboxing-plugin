package miniboxing.tests.compile.collctions1

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

  private var head: List[T] = Nil

  def +=(e1: T): Unit = head = e1 :: head
  def finalise: List[T] = head.reverse
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

  def sum[@miniboxed B >: T](implicit n : Numeric[B]): B = {
    var buff = n.zero
    foreach(new Function1[B,Unit] { def apply(b: B): Unit = buff = n.plus(buff,b) })
    buff
  }

  def foreach[@miniboxed U](f: Function1[T, U]): Unit
}



// Iterable
trait Iterable[@miniboxed +T] extends Traversable[T] {
  def iterator: Iterator[T]

  def zipTo[@miniboxed U, To](that: Iterable[U])(b: Builder[Tuple2[T, U], To]): To = {
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext() && those.hasNext()) {
      b += new Tuple2[T,U](these.next(),those.next())
    }
    b.finalise
  }

  def zip[@miniboxed U](that: Iterable[U]): List[Tuple2[T, U]] = zipTo[U, List[Tuple2[T, U]]](that)(new ListBuilder[Tuple2[T, U]])
}




// Iterator
trait Iterator[@miniboxed +T] {
  def hasNext(): Boolean
  def next(): T
}



// List
abstract class List[@miniboxed +T] extends Iterable[T] {

  def iterator = new Iterator[T] {
    var current = List.this
    def hasNext = current != Nil
    def next() = {
      val t = current.head
      current = current.tail
      t
    }
  }
  def foreach[@miniboxed U](f: Function1[T, U]) {
    val it = iterator
    while (it.hasNext) f(it.next())
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

case class ::[@miniboxed T](head: T, tail: List[T]) extends List[T] {
  def size = 1 + tail.size

  override def toString = head.toString + " :: " + tail.toString
}

case object Nil extends List[Nothing] {
  def head = throw new NoSuchElementException("head of empty list")
  def tail = throw new NoSuchElementException("tail of empty list")

  def size = 0

  override def toString = "Nil"
}



// Benchmarks

object LeastSquaresBenchmark extends App {

  val random = new scala.util.Random(0)

  // Random between (-1.0, 1.0), mean = 0
  def rand = random.nextDouble - random.nextDouble

  // Function to approximate = 5x + 3
  val step = 5.0
  val zero = 3.0
  val func = new Function1[Int, Double] {
    def apply(x: Int): Double = step*x + zero
  }

  implicit object Num_D extends Numeric[Double] {
    def plus(x: Double, y: Double): Double = x + y

    def zero: Double = 0.0
  }

  val size = 30000

  // generates random points from original function
  var listx: List[Double] = Nil
  var listy: List[Double] = Nil
  var i = 0
  while (i < size) {
    listx = (i + rand) :: listx
    listy = (func(i) + rand) :: listy
    i += 1
  }

  val listxy = listx.zip(listy)
  val sumx  = listx.sum
  val sumy  = listy.sum

  // function (x, y) => x * y
  val fxy = new Function1[Tuple2[Double,Double], Double] {
    def apply(t: Tuple2[Double, Double]): Double = t._1 * t._2
  }
  val sumxy = listxy.map(fxy).sum

  // function x => x * x
  val fxx = new Function1[Double, Double] {
    def apply(x: Double): Double = x * x
  }
  val squarex = listx.map(fxx).sum

  val m = (size*sumxy - sumx*sumy) / (size*squarex - sumx*sumx)
  val b = (sumy*squarex - sumx*sumxy) / (size*squarex - sumx*sumx)

  // was it a good approximation?
  assert(m - step < 0.1, "m exceeded 10% of error : " + m + " instead of " + step)
  assert(b - zero < 0.1, "b exceeded 10% of error : " + b + " instead of " + zero)
}
