package sample.scala.collection.mutable

import sample.scala.collection.immutable._

class ListBuffer[A] {
  private var start: List[A] = Nil
  private var last0: ::[A] = _
  private var len = 0

  def ++=(xs: List[A]): this.type = {
    if (!xs.isEmpty) {
      if (start == Nil) {
        start = xs
        last0 = xs.asInstanceOf[::[A]]
      } else {
        last0.tl = xs
      }
      while (!last0.tl.isEmpty) {
        last0 = last0.tl.asInstanceOf[::[A]]
      }
    }
    this
  }

  def prependToList(xs: List[A]): List[A] = {
    last0.tl = xs
    this.start
  }
}

