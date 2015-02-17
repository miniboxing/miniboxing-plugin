package miniboxing.runtime.array;

import miniboxing.runtime.*;
import scala.MbArray;

public class MbArrayOpts {

  public static <T> long mbArray_apply_J(MbArray<T> mbArray, int index, byte T_Tag) {
    if (mbArray instanceof MbArray_J<?>)
      return ((MbArray_J<?>)mbArray).apply_J(index);
    else
      return MiniboxConversionsLong.box2minibox_tt(mbArray.apply(index), T_Tag);
  }

  public static <T> double mbArray_apply_D(MbArray<T> mbArray, int index, byte T_Tag) {
    if (mbArray instanceof MbArray_D<?>)
      return ((MbArray_D<?>)mbArray).apply_D(index);
    else
      return MiniboxConversionsDouble.box2minibox_tt(mbArray.apply(index), T_Tag);
  }

  public static <T> void mbArray_update_J(MbArray<T> mbArray, int index, long value, byte T_Tag) {
    if (mbArray instanceof MbArray_J<?>)
      ((MbArray_J<?>)mbArray).update_J(index, value);
    else
      mbArray.update(index, MiniboxConversionsLong.<T>minibox2box(value, T_Tag));
  }

  public static <T> void mbArray_update_D(MbArray<T> mbArray, int index, double value, byte T_Tag) {
    if (mbArray instanceof MbArray_D<?>)
      ((MbArray_D<?>)mbArray).update_D(index, value);
    else
      mbArray.update(index, MiniboxConversionsDouble.<T>minibox2box(value, T_Tag));
  }

  public static <T> MbArray<T> mbArray_empty_J(int size, byte T_Tag) {
    return new MbArray_J<T>(T_Tag, size);
  }

  public static <T> MbArray<T> mbArray_empty_D(int size, byte T_Tag) {
    return new MbArray_D<T>(T_Tag, size);
  }

  public static <T> MbArray<T> mbArray_clone_J(Object array, byte T_Tag) {
    return new MbArray_J<T>(T_Tag, array);
  }

  public static <T> MbArray<T> mbArray_clone_D(Object array, byte T_Tag) {
    return new MbArray_D<T>(T_Tag, array);
  }
}