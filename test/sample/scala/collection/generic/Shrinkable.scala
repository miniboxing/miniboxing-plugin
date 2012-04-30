package sample.scala.collection.generic
import sample.scala.collection.immutable._

trait Shrinkable[-A] {
  def --(): this.type
  def -=(elem: A): this.type = --
}




