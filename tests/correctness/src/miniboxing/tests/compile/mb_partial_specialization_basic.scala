package miniboxing.tests.compile



class UhOh1[@miniboxed X, Y]{
  def foo(x: X, y: Y): X = foo(x, y)
}
