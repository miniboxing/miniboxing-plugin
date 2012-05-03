
package sample.scala.collection.generic

import sample.scala.collection.immutable._

trait Growable[-A] {
  def +=(elem: A): this.type
  def ++=(xs: List[A]): this.type = { xs foreach += ; this }
  def clear()
}
