package miniboxing.tests.compile.erasure12

object Test {
  def foo[@miniboxed T](a: T, b: T) =
    // in this expression the boxing should happen
    // just before going into the toString call, not
    // before, in the branches of the if statement
    (if (a == b) a else b).toString
}

