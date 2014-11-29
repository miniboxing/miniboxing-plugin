package miniboxing.tests.correctness.bug114

class C[@miniboxed T](t: T) {
  def method() = {
    // the body of foo is _not_ specialized since T => T is not an
    // optimized stack initiator, at least not in specialization's view:
    val foo = (x: T) => x
    foo(t)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    println(new C("3").method())
    println(new C(300).method())
    println(new C(3.0).method())
  }
}
