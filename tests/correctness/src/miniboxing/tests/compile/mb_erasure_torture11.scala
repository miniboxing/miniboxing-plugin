package miniboxing.tests.compile.erasure11

class CCCCC[@miniboxed T] {
  def test(c: T) = {
    (if (c.hashCode() == 0)
      c
    else
      ???).toString
  }
}
