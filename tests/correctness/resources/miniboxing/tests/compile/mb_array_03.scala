package miniboxing.tests.compile.mbarray03

class Vec[@miniboxed T](mbArray: MbArray[T]) {
  def foo = {
    // should rewire:
    val t0 = mbArray(0)
    val t1 = mbArray(1)
    mbArray(0) = t1
    mbArray(1) = t0
    println(mbArray(0))
    println(mbArray(1))
  }
}

object Vec {
  def apply[@miniboxed T](array: Array[T]) = new Vec(MbArray.clone(array))
}

object Test extends App {
  Vec(Array(1, 2, 3, 4, 5)).foo
  Vec(Array("x", "y", "z")).foo
  Vec(Array(1.0, 2.0, 3.0)).foo
  Vec(Array(true, false)).foo
}
