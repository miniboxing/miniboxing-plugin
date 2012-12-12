package miniboxing.sample.scala.collection.generic
import miniboxing.sample.scala.collection.immutable._

trait Shrinkable[-A] {
  def --(): this.type
  def -=(elem: A): this.type = --
}




