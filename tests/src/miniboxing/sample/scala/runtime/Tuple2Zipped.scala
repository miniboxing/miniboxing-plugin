package miniboxing.sample.scala.runtime


import miniboxing.sample.scala.collection.{ TraversableLike => TLike, IterableLike => ILike }
import miniboxing.sample.scala.collection.generic.{ CanBuildFrom => CBF }

final class Tuple2Zipped[+Repr1, +El1, +Repr2, +El2](
    coll1: TLike[El1, Repr1],
    coll2: ILike[El2, Repr2]) {

  def map[B, To](f: (El1, El2) => B)(implicit cbf: CBF[Repr1, B, To]): To = {
    val b = cbf(coll1.repr)
    val elems2 = coll2.iterator

    for (el1 <- coll1) {
      if (elems2.hasNext)
        b += f(el1, elems2.next)
      else
        return b.result
    }

    b.result
  }
}
