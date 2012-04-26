package runtime

object IntSnippet extends Snippet {
  override def arrayGet(array: AnyRef, i: Int): Long = 
    array.asInstanceOf[Array[Int]](i).asInstanceOf[Long]
  
  override def arrayUpdate(array: AnyRef, i: Int, e: Long): Unit =
    array.asInstanceOf[Array[Int]](i) = e.asInstanceOf[Int]
  
  override def arrayCreate(size: Int): AnyRef =
    new Array[Int](size)
  
  override def hashhash(x: Long): Int = 
    x.asInstanceOf[Int]
  
  override def toString(x: Long): String = 
    Integer.toString(x.asInstanceOf[Int])
  
  override def equals(x: Long, y: Long, ys: Snippet) =
    if (ys == this) x == y else false
}