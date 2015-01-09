package miniboxing.tests.compile.bug169

// AnyRef traits:
trait A[@miniboxed T] {
  println("A init")
}

trait B[@miniboxed T] extends A[T] {
  println("B init")
}

trait C[@miniboxed T] {
  println("C init")
}

class D[@miniboxed T] extends C[T] with B[T] {
  println("D init")
}


// universal (Any) traits:
trait AA[@miniboxed T] extends Any {
  def foo = 3
}

trait BB[@miniboxed T] extends Any with AA[T] {
  override def foo = 4
}

trait CC[@miniboxed T] extends Any {
  def bar = 3
}

class DD[@miniboxed T] extends CC[T] with BB[T] {
  println(foo)
  println(bar)
}

object Test {
  def main(args: Array[String]): Unit = {
    new D[Int]  // using AnyRef traits
    new DD[Int] // uning universal traits
  }
}
