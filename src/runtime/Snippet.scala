package runtime

trait Snippet {
  def arrayGet(array: AnyRef, i: Int): Long 
  
  def arrayUpdate(array: AnyRef, i: Int, e: Long): Unit
  
  def arrayCreate(size: Int): AnyRef
  
  def hashhash(x: Long): Int
  
  def toString(x: Long): String
  
  def equals(x: Long, y: Long, ys: Snippet): Boolean
}