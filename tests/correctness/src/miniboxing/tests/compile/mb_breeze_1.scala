package miniboxing.tests.compile
import miniboxing.plugin.minispec

trait TensorLike[@minispec K, @minispec V, +This]
trait VectorLike[@minispec E, +Self <: Vector[E]] extends TensorLike[Int, E, Self]
trait Vector[@minispec E] extends VectorLike[E, Vector[E]]
