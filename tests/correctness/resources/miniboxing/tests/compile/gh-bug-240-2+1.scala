package miniboxing.compile.tests.bug240.full

trait Monoid[@miniboxed M] {
  def op(m: M, n: M): M
  def id: M
  def combineN(x: M, n: Int): M = { // performs binary exponentiation
    if (n == 0) return id
    var y = x
    var m = n
    while (m % 2 == 0) {
      m >>= 1
      y = op(y, y)
    }
    var r = y
    while (m > 1) {
      m >>= 1
      y = op(y, y)
      if (m % 2 == 1)
        r = op(r, y)
    }
    r
  }
}

trait EndofunctionSpace[@miniboxed X] {
  def asMonoid: Monoid[X => X] = new Monoid[X => X] {
    def op(f: X => X, g: X => X) = f compose g
    def id = x => x
  }
  def composeN(f: X => X, n: Int) = asMonoid.combineN(f, n)
}
