package miniboxing.tests.compile.bug51

class C[@miniboxed T](val t: T) {
  def foo[@miniboxed U](u: U) =
    t == u
}
