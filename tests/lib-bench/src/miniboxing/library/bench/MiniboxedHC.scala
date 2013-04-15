package miniboxing.library.bench

object MiniboxedHC {
  /* TODO: Fill in with actual class */
  type List[T] = scala.collection.immutable.List[T]
  def getList(size: Int): List[Float] = (1 to size).map(_.toFloat).toList
}
