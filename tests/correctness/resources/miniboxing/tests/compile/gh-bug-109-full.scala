package miniboxing.example

// Function
trait Function1[@miniboxed -T, @miniboxed +S] {
  def apply(t: T): S
}

trait Function2[@miniboxed -T1, @miniboxed -T2, @miniboxed +R] {
  def apply(t1: T1, t2: T2): R
}



// Tuple
case class Tuple2[@miniboxed +T1, @miniboxed +T2](_1: T1, _2: T2) {
  override def toString() = "(" + _1.toString + "," + _2.toString + ")"
}



// Growable
trait Growable[@miniboxed -T] {
  
  def += (e1: T): Unit
  def ++= (xs: Traversable[T]): this.type = {
    def loop(xs: LinearSeqOptimized[T]) {
      if (xs.nonEmpty) {
        this += xs.head
        loop(xs.tail)
      }
    }
    xs match {
      case xs: LinearSeqOptimized[_] => loop(xs)
      case xs                        => {
        val func = new Function1[T, Unit] { def apply(t: T): Unit = +=(t) }
        xs.foreach(func)
      }
    }
    this
  }
}



// Builder
trait Builder[@miniboxed -T, +To] extends Growable[T] {

  def finalise: To
}

// Can build from implicit
trait CanBuildFrom[-From, -Elem, +To] {

  def apply(): Builder[Elem, To]
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
trait Traversable[@miniboxed +T] extends TraversableLike[T, Traversable[T]]

trait TraversableLike[@miniboxed +T, +Repr] {

  def mapTo[@miniboxed U, To](f: Function1[T, U])(b: Builder[U, To]): To = {
    foreach(new Function1[T,Unit] { def apply(t: T): Unit = b += f(t) })
    b.finalise
  }
  
  def flatMapTo[@miniboxed U, To](f: Function1[T, Traversable[U]])(b: Builder[U, To]): To = {
    foreach(new Function1[T,Unit] { def apply(t: T): Unit = b ++= f(t) })
    b.finalise
  }

  def map[@miniboxed U, That](f: Function1[T, U])(implicit cbf: CanBuildFrom[Repr, U, That]): That = mapTo[U, That](f)(cbf())

  def flatMap[@miniboxed U, That](f: Function1[T, Traversable[U]])(implicit cbf: CanBuildFrom[Repr, U, That]): That = flatMapTo[U, That](f)(cbf())

  def sum[@miniboxed B >: T](implicit n : Numeric[B]): B = foldLeft(n.zero) {
    new Function2[B, T, B] { def apply(b: B, t: T): B = n.plus(b, t) }
  }

  def foreach[@miniboxed U](f: Function1[T, U]): Unit

  def foldLeft[@miniboxed B](z: B)(op: Function2[B, T, B]): B
}



// Iterable
trait Iterable[@miniboxed +T] extends Traversable[T] {
  def iterator: Iterator[T]
}

trait IterableLike[@miniboxed +T, +Repr] extends Traversable[T] {

  def iterator: Iterator[T]

  def zipTo[@miniboxed B, To](that: Iterable[B])(b: Builder[Tuple2[T, B], To]): To = {
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      b += (new Tuple2(these.next, those.next))
    b.finalise
  }

  def zip[@miniboxed U, That](that: Iterable[U])(implicit cbf: CanBuildFrom[Repr, Tuple2[T, U], That]): That = zipTo[U, That](that)(cbf())
}


// Iterator
trait Iterator[@miniboxed +T] {
  def hasNext(): Boolean
  def next(): T
}


trait LinearSeqOptimized[@miniboxed +A] extends Iterable[A] {

  def isEmpty: Boolean
  
  def nonEmpty: Boolean = !isEmpty

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
  def foldLeft[@miniboxed B](z: B)(f: Function2[B, A, B]): B = {
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
abstract class List[@miniboxed +T] extends Traversable[T] with TraversableLike[T, List[T]] with Iterable[T] with IterableLike[T, List[T]] with LinearSeqOptimized[T] {

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

object List {
  implicit def canBuildFrom[@miniboxed A]: CanBuildFrom[List[_], A, List[A]] =
    new CanBuildFrom[List[_], A, List[A]] {
      def apply = new ListBuilder[A]
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

object Dice extends App {
  
  val dice = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
  
  var result = dice
  var rolls = 8
  
  while (rolls > 0) {
    
    val f = new Function1[Int, List[Int]] {
      def apply(t1: Int): List[Int] = {
        val f0 = new Function1[Int, Int] {
          def apply(t2: Int): Int = t1 + t2
        }
        dice.map(f0)
      }
    }
    result = result.flatMap(f)
  }
}


/*
 * To have a look afterwards : Map benchmark with telephone example
 */

//package week6
//
//import scala.io.Source
//
//object x {
//  val mnem = Map(
//    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
//    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")
//                                                  //> mnem  : scala.collection.immutable.Map[Char,String] = Map(8 -> TUV, 4 -> GHI
//                                                  //| , 9 -> WXYZ, 5 -> JKL, 6 -> MNO, 2 -> ABC, 7 -> PQRS, 3 -> DEF)
//
//  /** Invert the mnem map to give a map from chars 'A' ... 'Z' to '2' ... '9' */
//  val charCode: Map[Char, Char] =
//    for ((digit, str) <- mnem; ltr <- str) yield (ltr -> digit)
//                                                  //> charCode  : Map[Char,Char] = Map(E -> 3, X -> 9, N -> 6, T -> 8, Y -> 9, J -
//                                                  //| > 5, U -> 8, F -> 3, A -> 2, M -> 6, I -> 4, G -> 4, V -> 8, Q -> 7, L -> 5,
//                                                  //|  B -> 2, P -> 7, C -> 2, H -> 4, W -> 9, K -> 5, R -> 7, O -> 6, D -> 3, Z -
//                                                  //| > 9, S -> 7)
//
//  /** Maps a word to the digit string it can represent, e.g. "Java" -> "5282" */
//  def wordCode(word: String): String =
//    word.toUpperCase.map(charCode)                //> wordCode: (word: String)String
//
//  wordCode("scalaisfun")                          //> res0: String = 7225247386
//
//  object H {
//    val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt").getLines
//    val words = in.toList filter (_ forall (_.isLetter))
//    val wordsForNum: Map[String, Seq[String]] =
//      words groupBy wordCode withDefaultValue List()
//  }
//  import H._
//
//  /**
//   * A map from digit strings to the words that represent them,
//   * e,g. "5282" -> List("Java", "Kata", "Lava", ...)
//   * Note: A missing number should map to the empty set, e.g. "1111" -> List()
//   */
//  //val wordsForNum: Map[String, Seq[String]] =
//  //  words groupBy wordCode withDefaultValue List()
//
//  /** Return all ways to encode a number as a list of words */
//  def encode(number: String): Set[List[String]] = {
//    if (number.isEmpty) Set(List())
//    else {
//      for {
//        split <- 1 to number.length
//        first <- wordsForNum(number take split)
//        rest <- encode(number drop split)
//      } yield first :: rest
//    }.toSet
//  }                                               //> encode: (number: String)Set[List[String]]
//
//  encode("7225247386")                            //> res1: Set[List[String]] = Set(List(rack, ah, re, to), List(sack, ah, re, to
//                                                  //| ), List(Scala, ire, to), List(sack, air, fun), List(rack, air, fun), List(r
//                                                  //| ack, bird, to), List(pack, air, fun), List(pack, ah, re, to), List(pack, bi
//                                                  //| rd, to), List(Scala, is, fun), List(sack, bird, to))
//}
