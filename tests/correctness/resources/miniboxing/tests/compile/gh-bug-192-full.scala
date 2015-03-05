trait Monoid[@miniboxed X] {
  def id: X
  def op(x: X, y: X): X
}

trait EndofunctionSpace[@miniboxed X] {
  def asMonoidWithCompose: Monoid[X => X] = new Monoid[X => X] {
    def id = (x: X) => x
    def op(f: X => X, g: X => X) = (x: X) => f(g(x))
  }
}

object Test extends App {

  val fs = new EndofunctionSpace[Double] {}
  val m = fs.asMonoidWithCompose
  val f = math.sin _
  val g = math.asin _
  val fg = m.op(f, g)
  val fg1 = fg(1)
  println(fg1)
}
