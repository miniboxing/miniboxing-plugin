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
}


