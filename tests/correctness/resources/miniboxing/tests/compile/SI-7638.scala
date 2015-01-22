package miniboxing.tests.compile
 
trait Ordering[@specialized(Int) A] {
  def eqv(x: Array[A], y: Array[A]): Boolean = false
}
 
trait ArrayVectorOrder[@specialized(Int) A] extends Ordering[A] {
  override def eqv(x: Array[A], y: Array[A]): Boolean = super.eqv(x, y)
}
 
object vectorOrder {
  implicit def arrayOrder[@specialized(Int) A]() = new ArrayVectorOrder[A] { }
}
