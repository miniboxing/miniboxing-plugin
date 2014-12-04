package scala;

import miniboxing.runtime.array.MbAnyRefArray;

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
abstract class MbArray[T] {

  /** Get an element from the array. */
  def apply(idx: Int): T

  /** Update an element in the array. */
  def update(idx: Int, t: T): Unit

  /** Get the array length. */
  def length(): Int

  /** Clone the current array */
  def clone(): MbArray[T]

  // TODO: It may be cool to have this additional member to
  // allow cloning the array outside miniboxed code :)
  // def newArrayOfSize(size: Int): MbArray[T]
}

object MbArray {

  /** Create an empty MbArray of `size` */
  def empty[T](size: Int): MbArray[T] =
    new MbAnyRefArray[T](new Array[AnyRef](size));

  /** Clone an array into a MbArray */
  def cloneArray[T](array: Array[T]): MbArray[T] =
    new MbAnyRefArray[T](array);
}