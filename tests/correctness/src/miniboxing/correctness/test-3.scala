package correctness


/* This code tests the order of replacing symbols - it should generate valid code */
class Test3[@miniboxed T] {
  val t: T = ???
  def foo() = {
    // how is List[T] replaced?
    List(t,t,t)
  }
}
