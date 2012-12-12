package miniboxing.sample.scala.collection

trait LinearSeqOptimized[+A, +Repr <: LinearSeqOptimized[A, Repr]] { self: Repr =>
  def isEmpty: Boolean
  def head: A
  def tail: Repr

  def foreach[B](f: A => B) {
    var these = this
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }

  def exists(p: A => Boolean): Boolean = {
    var these = this
    while (!these.isEmpty) {
      if (p(these.head)) return true
      these = these.tail
    }
    false
  }

  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    var acc = z
    var these = this
    while (!these.isEmpty) {
      acc = f(acc, these.head)
      these = these.tail
    }
    acc
  }

  def reduceLeft[B >: A](f: (B, A) => B): B =
    if (isEmpty) throw new UnsupportedOperationException("empty.reduceLeft")
    else tail.foldLeft[B](head)(f)
}
