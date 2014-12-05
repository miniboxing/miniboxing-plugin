package miniboxing.tests.compile.mbarray02
import scala.language.existentials
import reflect.ClassTag

object Test extends App {
  // no warnings
  val mba0 = MbArray.empty[Int](10)
  val mba1 = MbArray.clone(new Array[Int](10))
  val mba2 = MbArray.clone(new Array[Double](10))
  val mba3 = MbArray.clone(new Array[String](10))
  def foo[@miniboxed T: ClassTag] = MbArray.clone(new Array[T](10))
  // warnings
  val mba4 = MbArray.clone(new Array[Any](10))
  val mba5 = MbArray.clone(new Array[F forSome { type F }](10))
  def bar[V: ClassTag] = MbArray.clone(new Array[V](10))
}
