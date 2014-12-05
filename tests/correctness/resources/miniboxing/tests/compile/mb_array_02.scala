package miniboxing.tests.compile.mbarray02
import scala.language.existentials

object Test extends App {
  // no warnings
  val mba1 = MbArray.empty[Int](10)
  val mba2 = MbArray.empty[Double](10)
  val mba3 = MbArray.empty[String](10)
  def foo[@miniboxed T] = MbArray.empty[T](10)
  // warnings
  val mba4 = MbArray.empty[Any](10)
  val mba5 = MbArray.empty[F forSome { type F }](10)
  def bar[V] = MbArray.empty[V](10)
}
