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
class UhOh[@miniboxed X, Y]{
  def foo(x: X, y: Y): X = foo(x, y)
}

class OhUh[@miniboxed U](var u: U) {
  def test = {
    val tr = new UhOh[U, Int] // case (2.4)
    tr.foo(u, 3)
  }
}
