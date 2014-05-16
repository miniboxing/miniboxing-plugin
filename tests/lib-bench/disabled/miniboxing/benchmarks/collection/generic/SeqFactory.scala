package miniboxing.benchmarks.collection.generic
import scala.language.higherKinds
import miniboxing.benchmarks.collection.Seq

abstract class SeqFactory[CC[X] <: Seq[X] with GenericTraversableTemplate[X, CC]]
extends GenSeqFactory[CC] with TraversableFactory[CC] {

  /** This method is called in a pattern match { case Seq(...) => }.
   *
   *  @param x the selector value
   *  @return  sequence wrapped in an option, if this is a Seq, otherwise none
   */
  def unapplySeq[A](x: CC[A]): Some[CC[A]] = Some(x)
}
