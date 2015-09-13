package miniboxing.compile.tests.bug240

class EndofunctionSpace[@miniboxed X] {
  def composeN(f: X => X, n: Int): X => X = ???
}
