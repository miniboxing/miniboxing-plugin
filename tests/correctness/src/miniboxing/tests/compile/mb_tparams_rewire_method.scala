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
class TR3[@miniboxed T, Z] {
  def foo[X, Y](t: T, x: X, y: Y): T = foo(t, y, x)        // case (1)
  def bar[X, Y](t: TR3[T,Z], x: X, y: Y) = t.foo(???, x, y) // case (2.4)
}

class RT3[@miniboxed U, G](g: G) {
  def test(u: U) = {
    val tr = new TR3[U, Int]
    tr.foo[G, Any](u, g, g)
  }
}

object Test3 {
  def test[T, A](a: A) = {
    (new TR3[Int, String]).foo(3, a, a)                     // case (2.1)
    (new TR3[String, Int]).foo("xxx", a, 4)                 // case (2.2)
    (new TR3[T, Long]).foo[Int, Any](???, 2, 3)             // case (2.3)
  }
}
