package miniboxing.benchmarks.collection
package generic

import scala.language.higherKinds

/** A template for companion objects of Seq and subclasses thereof.
 *
 *  @since 2.8
 */
abstract class GenSeqFactory[CC[X] <: GenSeq[X] with GenericTraversableTemplate[X, CC]]
extends GenTraversableFactory[CC]
