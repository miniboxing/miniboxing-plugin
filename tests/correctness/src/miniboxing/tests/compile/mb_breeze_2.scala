package miniboxing.tests.compile.breeze2
import miniboxing.plugin.minispec

trait TensorLike[@minispec K, @minispec V, +This]
trait VectorLike[@minispec E, +Self <: Vector[E]] extends TensorLike[Int, E, Self]
trait Vector[@minispec E] extends VectorLike[E, Vector[E]] {
  def toDenseVector() = {
    new DenseVector(toArray)
  }
  def toArray: Array[E] = ???
}

trait Storage[@minispec Elem]
trait StorageVector[E] extends Vector[E] with Storage[E]

class DenseVector[@minispec E](val data: Array[E],
                               val offset: Int,
                               val stride: Int,
                               val length: Int) extends StorageVector[E]
                               with VectorLike[E, DenseVector[E]] with Serializable {
  def this(data: Array[E]) = this(data, 0, 1, data.length)
  def this(data: Array[E], offset: Int) = this(data, offset, 1, data.length)
}
