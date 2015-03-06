trait A[X] {
  def m(x: X): X
}
trait B[V] extends A[V] {
  def m(x: V): V
}
trait C[X, Y] extends B[X => Y] {
  def m(f: X => Y): X => Y = ???
}
