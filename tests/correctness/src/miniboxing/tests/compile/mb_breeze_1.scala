package miniboxing.tests.compile


trait TensorLike[@miniboxed K, @miniboxed V, +This]
trait VectorLike[@miniboxed E, +Self <: Vector[E]] extends TensorLike[Int, E, Self]
trait Vector[@miniboxed E] extends VectorLike[E, Vector[E]]
