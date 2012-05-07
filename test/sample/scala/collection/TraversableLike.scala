package sample.scala.collection

import generic.FilterMonadic
import generic.CanBuildFrom
import mutable.Builder
import mutable.ListBuffer
import immutable._
import generic.Growable
import generic.CanBuildFrom
import scala.runtime.ObjectRef


trait TraversableLike[+A, +Repr] extends FilterMonadic[A, Repr]{
  def repr: Repr = this.asInstanceOf[Repr]
  def foreach[U](f: A => U): Unit

  def toList(): List[A] = {
    var lb = new ObjectRef(new ListBuffer[A]())
    this foreach new AppendToBuffer[A](lb)
    lb.elem.prependToList(Nil)
  }
  
  def ++[B >: A, That](that: List[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    b ++= toList
    b ++= that
    b.result
  }
   
  def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    for (x <- this) b += f(x)
    b.result
  }
}

class AppendToBuffer[A](lb: ObjectRef[ListBuffer[A]]) extends Function1[A, Unit]{
  override def apply(a: A): Unit = {
    lb.elem ++= List(a)
  }
}