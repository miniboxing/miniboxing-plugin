//
//     _____   .__         .__ ____.                     .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_  |__    _____  ___  ___|__|  ____    _____
//   /  \ /  \ |  | /    \ |  | |  __ \  /  ___\ \  \/  /|  | /    \  /  ___\
//  /    Y    \|  ||   |  \|  | |  \_\ \(  (_)  ) >    < |  ||   |  \(  /_/  )
//  \____|__  /|__||___|  /|__| |____  / \_____/ /__/\_ \|__||___|  / \___  /
//          \/          \/           \/                \/         \/ /_____/
// Copyright (c) 2011-2015 Scala Team, École polytechnique fédérale de Lausanne
//
// Authors:
//    * Vlad Ureche
//    * Nicolas Stucki
//    * Milos Stojanovic
//
package scala

import miniboxing.internal.array.MbArray_L;

/**
 * The `scala.MbArray` class is an alternative implementation of the `scala.Array` class
 * that does not require a `scala.reflect.ClassTag` to be instantiated. On the other hand,
 * the locality and unboxing guarantees are still valid when used with the miniboxing
 * plugin. For example:
 *
 * ```
 *    class C[@miniboxed T] {
 *      val mb: MbArray[T] = MbArray.empty(100)
 *    }
 * ```
 *
 * If the instatiation is done outside miniboxed code or cannot be optimized, the miniboxing
 * plugin will warn (don't forget to add the `-P:minibox:warn` flag to your build!)
 * **Avoid the warnings at your own risk: You will lose the locality and unboxing guarantees!**
 */
abstract class MbArray[T] extends Serializable {

  /** Get an element from the array. */
  def apply(idx: Int): T

  /** Update an element in the array. */
  def update(idx: Int, t: T): Unit

  /** Get the array length. */
  def length(): Int

  /** Clone the current array */
  override def clone(): MbArray[T] = sys.error("Should be overridden")

  /** Array copy for MbArrays
   *  @see [[http://docs.oracle.com/javase/7/docs/api/java/lang/System.html System.arraycopy]] */
  protected def arraycopy(srcPos: Int, dest: MbArray[T], destPos: Int, length: Int): Unit = {
    val end = srcPos + length
    var i = srcPos
    var j = destPos
    while (i < end) {
      dest(j) = this(i)
      i += 1
      j += 1
    }
  }
}

object MbArray {
  /** Create an empty MbArray of `size` */
  def empty[T](size: Int): MbArray[T] =
    new MbArray_L[T](size);

  /** Element-by-element constructor */
  def apply[T](xs: T*): MbArray[T] = {
    val mb_array = MbArray.empty[T](xs.length)
    var i = 0
    for (x <- xs.iterator) { mb_array.update(i, x); i += 1 }
    mb_array
  }

  /** Clone an array into a MbArray */
  def clone[T](array: Array[T]): MbArray[T] =
    new MbArray_L[T](array);

  /** Array copy for MbArrays
   *  @see [[http://docs.oracle.com/javase/7/docs/api/java/lang/System.html System.arraycopy]] */
  def arraycopy[T](src: MbArray[T], srcPos: Int, dest: MbArray[T], destPos: Int, length: Int): Unit =
    src.arraycopy(srcPos, dest, destPos, length)
}