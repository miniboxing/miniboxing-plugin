package miniboxing.tests.compile.bug148

abstract class Iterator[@miniboxed T] {
  def next(): T
}

object Test {
  def foo[@miniboxed V](v: V) = new Iterator[V] {
    def next(): V = v
  }

  def main(args: Array[String]): Unit = {
    val it: Iterator[_] = foo[Char]('a')
    println(it.next()) // expected: 'a'
                       // printed:   97
  }
}
