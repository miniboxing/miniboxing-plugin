package miniboxing.benchmarks
package collection
package mutable
import generic._

/** A subtrait of scala.collection.IndexedSeq which represents sequences
 *  that can be mutated.
 *
 *  @since 2.8
 */
trait IndexedSeqOptimized[A, +Repr] extends Any with IndexedSeqLike[A, Repr] with collection.IndexedSeqOptimized[A, Repr]
