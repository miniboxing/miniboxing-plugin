//package miniboxing.runtime.java
//
//class MiniboxArrayWrapper {
//  def mbarray_new(len: Int, tag: Byte): Any =
//    MiniboxArray.mbarray_new(len, tag)
//
//  def mbarray_apply_minibox(array: Any, idx: Int, tag: Byte): Long =
//    MiniboxArray.mbarray_apply_minibox(array, idx, tag)
//
//  def mbarray_apply_box[T](array: Any, idx: Int, tag: Byte): T =
//    MiniboxArray.mbarray_apply_box(array, idx, tag)
//
//  def mbarray_update_minibox(array: Any, idx: Int, value: Long, tag: Byte): Unit =
//    MiniboxArray.mbarray_update_minibox(array, idx, value, tag)
//
//  def mbarray_update_box[T](array: Any, idx: Int, value: T, tag: Byte): Unit =
//    MiniboxArray.mbarray_update_box(array, idx, value, tag)
//
//  def mbarray_length(array: Any, tag: Byte): Int =
//    MiniboxArray.mbarray_length(array, tag)
//}
