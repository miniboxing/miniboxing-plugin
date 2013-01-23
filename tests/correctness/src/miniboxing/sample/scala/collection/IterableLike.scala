package miniboxing.sample.scala.collection

trait IterableLike[+A, +Repr] extends TraversableLike[A, Repr] {
self =>
  def iterator: Iterator[A]
}

trait Iterator[+A] {
  def hasNext() : Boolean
  def next() : A
}


