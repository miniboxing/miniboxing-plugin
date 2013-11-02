package miniboxing.tests.compile

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

trait Dist[@miniboxed A] { self =>
  def sample[CC[A] <: Iterable[A]](n: Int)(implicit a: A, cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] =
    cbf().result
}
