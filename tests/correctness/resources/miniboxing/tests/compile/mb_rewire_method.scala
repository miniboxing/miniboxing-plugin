package miniboxing.tests.compile



/**
 * We can distinguish 5 cases:
 * (1) selecting the method from the current object (this) ==>
 *     we use the partial specialization info and type tags from the current method
 * (2) selecting a method from a specialized interface with type parameters
 *     2.1. fixed, specializable (Int, Long)
 *     2.2. fixed, not specializable (String)
 *     2.3. from the current scope, but not specialized
 *     2.4. from the current scope, but specialized
 */
class TR[@miniboxed T] {
  def foo(t: T): T = foo(t)      // case (1)
  def bar(t: TR[T]) = t.foo(???) // case (2.4)
}

class RT[@miniboxed U] {
  def test(u: U) = {
    val tr = new TR[U]
    tr.foo(u)
  }
}

object Test {
  def test[T] = {
    (new TR[Int]).foo(3)         // case (2.1)
    (new TR[String]).foo("xxx")  // case (2.2)
    (new TR[T]).foo(???)         // case (2.3)
  }
}
