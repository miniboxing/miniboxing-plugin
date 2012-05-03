package sample.scala.collection.mutable

import sample.scala.collection.generic.Growable
import sample.scala.collection.TraversableLike
import sample.scala.collection.immutable._
import sample.scala.collection.generic.CanBuildFrom

trait Builder[-Elem, +To] extends Growable[Elem] {
  def +=(elem: Elem): this.type
  def clear()
  def result(): To
  def sizeHint(size: Int) {}
  
  def mapResult[NewTo](f: To => NewTo): Builder[Elem, NewTo] =
    new Builder[Elem, NewTo] with Proxy {
      val self = Builder.this
      def +=(x: Elem): this.type = { self += x; this }
      def clear() = self.clear()
      override def ++=(xs: List[Elem]): this.type = { self ++= xs; this }
      def result: NewTo = f(self.result)
    }
}


