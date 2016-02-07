trait Type[T]

sealed trait EList[T] { self =>

  def head(): Option[T] = None
  def tail(): EList[T] = Nil()
}

final case class Cons[T](hd: T, var tl: EList[T]) extends EList[T] {
  override def head() = Some(hd)
  override def tail() = tl
}

final case class Nil[T]() extends EList[T]

object EList {
  def apply[T](elems: T*): EList[T] = {
    Nil()
  }

  trait ~>[Constraint[_], Coll[_]]

  trait +>[Constraint[_], F[_]] {
    def flatMap[A, B: Constraint](fa: F[A])(fn: A => F[B])
                               (implicit E: Constraint ~> F): F[B]
  }

  final implicit class FlatmapOps[A, Constraint[_], F[_]](fa: F[A])
                                 (implicit F: Constraint +> F) {
    def flatMap[B: Constraint](fn: A => F[B])
                              (implicit E: Constraint ~> F): F[B] =
      F.flatMap(fa)(fn)
  }

  trait Sequence[S[_]] extends +>[Type, EList] with ~>[Type, S] {

    def flatMap[A, B: Type](fa: S[A])(fn: (A) => S[B])(implicit E: Type ~> S): S[B] = 
      Nil().asInstanceOf[S[B]]
  }

  implicit val EListIsQueueAndStack = new Sequence[EList]{}
}

object Test {
  import EList._
  implicit def anyIsType[T] = new Type[T] {}
  def main(args: Array[String]): Unit =
    println(EList(1,2,3).flatMap(x => EList(1)))
}
