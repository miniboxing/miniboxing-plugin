package miniboxing.tests.compile


trait Iterator[@miniboxed +A] {
  self =>

  def seq: Iterator[A] = this
  def hasNext: Boolean
  def next(): A
  def isEmpty: Boolean = !hasNext

  def filter(p: A => Boolean): Iterator[A] = new AbstractIterator[A] {
    private var hd: A = _
    private var hdDefined: Boolean = false

    def hasNext: Boolean = hdDefined || {
      do {
        if (!self.hasNext) return false
        hd = self.next()
      } while (!p(hd))
      hdDefined = true
      true
    }

    def next() = if (hasNext) { hdDefined = false; hd } else ???
  }
}

abstract class AbstractIterator[@miniboxed +A] extends Iterator[A]
