package miniboxing.tests.compile.tparams



class TParams1[@miniboxed T] {
  def foo[X](t: T, x: X) = 12
}
