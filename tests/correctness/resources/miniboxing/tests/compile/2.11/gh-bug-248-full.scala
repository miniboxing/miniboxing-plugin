import language.{higherKinds, implicitConversions}
import scala.collection.mutable
import scala.annotation.tailrec

object Test extends App {
  
  trait Type[T]

  trait Val[T] extends Type[T]

  trait Ref[T] extends AnyRef with Type[T]

  implicit def anyIsType[T] = new Type[T] {}
  implicit def refIsAnyRef[T <: AnyRef] = new Ref[T] {}

  trait ~>[Constraint[_], Coll[_]] {
    def build[T: Constraint](iter: Iter[T]): Coll[T]
  }

  trait Iter[A] {
    def hasNext(): Boolean
    def next(): A
  }

  trait Iterable[I[_]] extends Type[I[_]] {
    def iterator[A](col: I[A]): Iter[A]
    def reversedIterator[A](col: I[A]): Iter[A]
    def fold[A, B](col: I[A])(init: B)(f: (A, B) => B): B = {
      var buffer = init
      val it = iterator(col)
      while (it.hasNext()) buffer = f(it.next(), buffer)
      buffer
    }
  }

  trait |>[Constraint[_], M[_]] {

    def map[A, B: Constraint](m: M[A])(fn: A => B)
                           (implicit E: Constraint ~> M): M[B]
  }

  trait ->[Constraint[_], F[_]] extends |>[Constraint, F] {
    def applicate[A, B: Constraint](fns: F[A => B])(fa: F[A])
               (implicit E: Constraint ~> F): F[B]
  }

  trait +>[Constraint[_], F[_]] extends ->[Constraint, F] {

    def flatMap[A, B: Constraint](fa: F[A])(fn: A => F[B])
                               (implicit E: Constraint ~> F): F[B]

    def applicate[A, B: Constraint](fns: F[A => B])(fa: F[A])
               (implicit E: Constraint ~> F): F[B] =
      flatMap(fns)((f: A => B) => map(fa)(f))
  }

// Error origin
  final implicit class FlatmapOps[A, Constraint[_], F[_]](fa: F[A])
                                 (implicit F: Constraint +> F) {

    def flatMap[B: Constraint](fn: A => F[B])
                              (implicit E: Constraint ~> F): F[B] =
      F.flatMap(fa)(fn)

    def +>[B: Constraint](fn: A => F[B])
                         (implicit E: Constraint ~> F): F[B] =
      F.flatMap(fa)(fn)
  }

  trait Appendable[T[_]] extends Type[T[_]] {
    def append[A](a: T[A], b: T[A]): T[A]
  }

  trait Empty[E[_]] extends Type[E[_]] {
    def empty[A]: E[A]
  }

  trait Monoidal[M[_]] extends Appendable[M] with Empty[M] {
    def flatten[A](m: M[M[A]]): M[A]
  }

  trait Collection[Constraint[_], S[_]] extends Iterable[S]
                                         with +>[Constraint, S]
                                         with Monoidal[S] {

    def flatten[A](coll: S[S[A]]): S[A] = fold(coll)(empty[A])(append(_, _))
  }

  trait Sequence[S[_]] extends Collection[Type, S]
                               with ~>[Type, S] {

    def flatMap[A, B: Type](fa: S[A])(fn: (A) => S[B])
                           (implicit E: Type ~> S): S[B] =
      flatten(map(fa)(fn))
  }


sealed trait EList[T] { self =>

  def head(): Option[T] = None
  def tail(): EList[T] = Nil()

  def insert(elem: T): EList[T] =
    if(this == Nil[T]()) Cons(elem, Nil())
    else {
      def rec(ls: EList[T]): EList[T] =
        ls match {
          case Cons(hd, tl) => Cons(hd, rec(tl))
          case Nil()        => Cons(elem, Nil())
        }
      rec(this)
    }

  def + = insert _

  def iterator() = new Iter[T] {
    private var ls: EList[T] = self
    def hasNext() = ls != Nil()
    def next() = {
      val value = ls.asInstanceOf[Cons[T]].head().get
      ls = ls.tail()
      value
    }
  }

  def reversedIterator() = new Iter[T] {
    private var ls: EList[T] = reverse()
    def hasNext() = ls != Nil()
    def next() = {
      val value = ls.asInstanceOf[Cons[T]].head().get
      ls = ls.tail()
      value
    }
  }

  def reverse(): EList[T] = {
    @tailrec def rec(buffer: EList[T], ls: EList[T]): EList[T] =
      ls match {
        case Cons(hd, tl) => rec(Cons(hd, buffer), tl)
        case Nil()        => buffer
      }
    rec(Nil(), this)
  }

  def filter(pred: T => Boolean): EList[T] =
    if(this == Nil[T]()) Nil()
    else {
      def rec(ls: EList[T]): EList[T] =
        ls match {
          case Cons(hd, tl) =>
            if(pred(hd)) Cons(hd, rec(tl)) else rec(tl)
          case Nil()        => ls
        }
      rec(this)
    }

  def size(): Int = {
    @tailrec def rec(EList: EList[T], n: Int): Int =
      EList match {
        case Cons(hd, tl) => rec(tl, n + 1)
        case Nil()        => n
      }
    rec(this, 0)
  }

  def apply(n: Int): Option[T] =
    if(n < 0) None
    else {
      @tailrec def rec(EList: EList[T], i: Int): Option[T] =
        if(i > n) None
        else EList match {
          case Cons(hd, tl) => if(i == n) Some(hd) else rec(tl, i + 1)
          case Nil()        => None
        }
      rec(this, 0)
    }

  def isEmpty() = this == Nil()

  def append(EList2: EList[T]): EList[T] =
   if(isEmpty()) EList2
   else if(EList2.isEmpty()) this
   else {
     def rec(EList: EList[T]): EList[T] =
       EList match {
         case Cons(hd, tl) => Cons(hd, rec(tl))
         case Nil()        => EList2
       }
     rec(this)
   }

  def map[B](fn: T => B): EList[B] =
    if(isEmpty()) Nil()
    else {
      var buffer: EList[B] = Nil()
      var bufferTail: EList[B] = Nil()

      @tailrec def rec(EList: EList[T]): Unit =
        EList match {
          case Cons(hd, tl) => {
            (buffer, bufferTail) match {
              case (Cons(h0, t0), Cons(h1, Nil())) => {
                bufferTail.asInstanceOf[Cons[B]].tl = Cons(fn(hd), Nil())
                bufferTail = bufferTail.tail()
              }
              case (Cons(h, Nil()), Nil()) => {
                buffer.asInstanceOf[Cons[B]].tl = Cons(fn(hd), Nil())
                bufferTail = buffer.tail()
              }
              case (Nil(), Nil()) => buffer = Cons(fn(hd), Nil())
              case _ => ()
            }
            rec(tl)
          }
          case Nil()        => ()
        }
      rec(this)
      buffer
    }

  def push(elem: T): EList[T] = Cons(elem, this)

  private def fromMutableHashSet(hs: mutable.HashSet[T]): EList[T] = {
    val it = hs.iterator

    def buildEList(): EList[T] =
      if(!it.hasNext) Nil()
      else Cons(it.next, buildEList())

    buildEList()
  }

  private def addToMutableHashSet(hs: mutable.HashSet[T], it: Iter[T]): Unit = {
    @tailrec def rec(): Unit =
      if(it.hasNext()) {
        hs.add(it.next)
        rec()
      }
    rec()
  }

}

final case class Cons[T](hd: T, var tl: EList[T]) extends EList[T] {
  override def head() = Some(hd)
  override def tail() = tl
}

final case class Nil[T]() extends EList[T]

object EList {
  def apply[T](elems: T*): EList[T] = {
    var (ls, i): (EList[T], Int) =(Nil(), elems.size - 1)

    while(i >= 0) {
      ls = Cons(elems(i), ls)
      i -= 1
    }

    ls
  }

  def fill[T](n: Int)(f: => T): EList[T] = {
    def rec(i: Int): EList[T] =
      if(i > n) Nil() else Cons(f, rec(i + 1))
    rec(1)
  }

  def build[I: Type](iter: Iter[I]): EList[I] = {
    def rec(): EList[I] =
      if(iter.hasNext()) Cons(iter.next(), rec()) else Nil()
    rec()
  }

}

implicit val EListIsQueueAndStack = new Sequence[EList] {

    def iterator[T](coll: EList[T]): Iter[T] = coll.iterator
    def reversedIterator[T](coll: EList[T]): Iter[T] = coll.reversedIterator

    def append[T](a: EList[T], b: EList[T]): EList[T] = a.append(b)

    def empty[T]: EList[T] = EList()

    def build[I: Type](iter: Iter[I]): EList[I] = EList.build(iter)

    def index[T](coll: EList[T])(n: Int): Option[T] = coll(n)

    def tail[T](coll: EList[T]): EList[T] = coll.tail()
    def head[T](coll: EList[T]): Option[T] = coll.head()

    def filter[T](coll: EList[T])(pred: (T) => Boolean): EList[T] =
      coll.filter(pred)

    def insert[T](coll: EList[T], elem: T): EList[T] = coll.insert(elem)

    def size[T](coll: EList[T]): Int = coll.size()

    def fill[T](n: Int)(f: => T): EList[T] = EList.fill(n)(f)

    def map[T, B: Type](m: EList[T])(fn: (T) => B)
                       (implicit E: ~>[Type, EList]): EList[B] =
      m.map(fn)
  }

  println(EList(1, 2, 3).flatMap(x => EList(x - 1, x + 1)))
}
