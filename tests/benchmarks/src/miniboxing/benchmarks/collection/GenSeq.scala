package miniboxing.benchmarks.collection


import generic._


/** A trait for all sequences which may possibly
 *  have their operations implemented in parallel.
 *
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait GenSeq[+A]
extends GenSeqLike[A, GenSeq[A]]
   with GenIterable[A]
   with Equals
   with GenericTraversableTemplate[A, GenSeq]
{
  def seq: Seq[A]
  override def companion: GenericCompanion[GenSeq] = GenSeq
}


object GenSeq extends GenTraversableFactory[GenSeq] {
  implicit def canBuildFrom[A] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A] = ??? // Seq.newBuilder
}
