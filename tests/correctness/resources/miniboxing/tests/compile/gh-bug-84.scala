package miniboxing.tests.compile

class G

class Error[@miniboxed U, G](g: G) {
  def foo[X](x: X) = ???
  def test(u: U) =
    foo(g)
}
