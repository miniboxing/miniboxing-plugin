package miniboxing.sample.scala.collection
package generic

trait FilterMonadic[+A, +Repr] extends Any {
  def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That
  def foreach[U](f: A => U): Unit
}
