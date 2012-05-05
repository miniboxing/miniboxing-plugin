package runtime

/**
 * We replace
 *   x.toString()
 * by
 *   TypeTagDispatch.toString(x, xTag)
 * in the tree of the method.
 *
 * These definitions will be inlined later during 'inline' phase.
 * So, the size of the code in some class will increase by only a factor
 * proportional to the size of the biggest of these methods (in the worst case)
 */
object MiniboxTypeTagDispatch {
  import MiniboxTypes._
  @inline final def toString[T](x: T)(implicit tag: Manifest[T]): String = "" + x
  @inline final def hashhash[T](x: T)(implicit tag: Manifest[T]): Int = x.##
  @inline final def hashCode[T](x: T)(implicit tag: Manifest[T]): Int = x.hashCode

  @inline final def array_apply[T](array: Any, pos: Int)(implicit tag: Manifest[T]): T =
    array.asInstanceOf[Array[T]](pos)

  @inline final def array_update[T](array: Any, pos: Int, x: T)(implicit tag: Manifest[T]): Unit = {
    ()
  }

  @inline final def newarray(len: Int, tag: Tag): Any = ()
}