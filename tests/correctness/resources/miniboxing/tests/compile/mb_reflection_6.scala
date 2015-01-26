package miniboxing.tests.compile.bug171.test6

import MbReflection._

object Test {
  def foo[@miniboxed T, @miniboxed U] = {
    if (isMiniboxed[T] && isMiniboxed[U])
      println("miniboxed")
    else
      println("other")
  }
}
