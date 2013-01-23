package miniboxing.sample.scala.collection.mutable

import miniboxing.sample.scala.collection.generic.Growable
import miniboxing.sample.scala.collection.TraversableLike
import miniboxing.sample.scala.collection.immutable._
import miniboxing.sample.scala.collection.generic.CanBuildFrom

trait Builder[-Elem, +To] extends Growable[Elem] {
  def +=(elem: Elem): this.type
  def clear()
  def result(): To
  def sizeHint(size: Int) {}
}


