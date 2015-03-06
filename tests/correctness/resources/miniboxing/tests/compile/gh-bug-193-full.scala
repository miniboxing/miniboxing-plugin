trait Group[@miniboxed X] {
  def add(x: X, y: X): X
  def zero: X
  def neg(x: X): X
  def sub(x: X, y: X) = add(x, neg(y))
}
trait Ring[@miniboxed X] extends Group[X] {
  def mul(x: X, y: X): X
  def one: X
  def negOne = neg(one)
}
trait Module[@miniboxed V, @miniboxed R] extends Group[V] {
  def ringOfScalar: Ring[R]
  def scale(k: R, x: V): V
  def neg(x: V): V = scale(ringOfScalar.negOne, x)
}
trait FunctionSpace[@miniboxed X, @miniboxed Y, @miniboxed R] extends Module[X => Y, R] {
  def moduleOfCodomain: Module[Y, R]
  def scale(a: R, f: X => Y) = (x: X) => moduleOfCodomain.scale(a, f(x))
  override def neg(f: X => Y) = (x: X) => moduleOfCodomain.neg(f(x))
  def zero = (x: X) => moduleOfCodomain.zero
  def add(f: X => Y, g: X => Y) = (x: X) => moduleOfCodomain.add(f(x), g(x))
  override def sub(f: X => Y, g: X => Y) = (x: X) => moduleOfCodomain.sub(f(x), g(x))
}
