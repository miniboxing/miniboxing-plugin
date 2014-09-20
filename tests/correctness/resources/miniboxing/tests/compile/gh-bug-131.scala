package minboxing.tests.compile.bug131

case class C[@miniboxed T]()

class D[@miniboxed T] {
  def canEqual(`x$1`: Any): Boolean = `x$1`.isInstanceOf[D[_]]
}
