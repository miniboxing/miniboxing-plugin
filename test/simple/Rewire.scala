package simple

// XXX: rewiring does not work
trait MyFunction[-T, R] {
  def apply(t : T) : R
}

trait Rewire[A] {
  def elem : A
  def exists(f : MyFunction[A, Boolean]) : Boolean = {
    f(elem)
  }
}