import scala.reflect._

object Test {
  def foo[@miniboxed T : ClassTag](t: T) = {
    new Array[T](42) // should warn
    new Array[Int](42) // should warn
    Array[T](t,t,t,t,t) // should warn
  }
  def bar[T : ClassTag](t: T) = {
    new Array[T](42)
    Array[T](t,t,t,t,t)
  }
}