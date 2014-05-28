package miniboxing.tests.compile.bug98

trait Iterator[@miniboxed +T] {
  def next(): T
}

class List[@miniboxed +T](val t: T) {
  def iterator: Iterator[T] = new Iterator[T] {
    def next(): T = t
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    println((new List(3)).iterator.next())
  }
}
