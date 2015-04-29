package miniboxing.runtime.array;

import miniboxing.runtime.*;
import miniboxing.runtime.MiniboxConstants;
import scala.MbArray;

public class MbArrayOpts {

  public static final <T> long mbArray_apply_J(MbArray<T> mbArray, int index, byte T_Tag) {
    switch(T_Tag) {
    case MiniboxConstants.LONG:
      return ((MbArray_J<?>)mbArray).apply_J(index);
    case MiniboxConstants.INT:
      return ((MbArray_I<?>)mbArray).apply_J(index);
    case MiniboxConstants.SHORT:
      return ((MbArray_S<?>)mbArray).apply_J(index);
    case MiniboxConstants.CHAR:
      return ((MbArray_C<?>)mbArray).apply_J(index);
    case MiniboxConstants.BYTE:
      return ((MbArray_B<?>)mbArray).apply_J(index);
    case MiniboxConstants.BOOLEAN:
      return ((MbArray_Z<?>)mbArray).apply_J(index);
    case MiniboxConstants.UNIT:
      return ((MbArray_V<?>)mbArray).apply_J(index);
    default:
      return MiniboxConversionsLong.<T>box2minibox_tt(mbArray.apply(index), T_Tag);
    }
  }

  public static <T> double mbArray_apply_D(MbArray<T> mbArray, int index, byte T_Tag) {
    if (mbArray instanceof MbArray_D<?>)
      return ((MbArray_D<?>)mbArray).apply_D(index);
    else
      return MiniboxConversionsDouble.box2minibox_tt(mbArray.apply(index), T_Tag);
  }

  public static final <T> void mbArray_update_J(MbArray<T> mbArray, int index, long value, byte T_Tag) {
    switch(T_Tag) {
      case MiniboxConstants.LONG:
        ((MbArray_J<?>)mbArray).update_J(index, value);
        break;
      case MiniboxConstants.INT:
        ((MbArray_I<?>)mbArray).update_J(index, value);
        break;
      case MiniboxConstants.SHORT:
        ((MbArray_S<?>)mbArray).update_J(index, value);
        break;
      case MiniboxConstants.CHAR:
        ((MbArray_C<?>)mbArray).update_J(index, value);
        break;
      case MiniboxConstants.BYTE:
        ((MbArray_B<?>)mbArray).update_J(index, value);
        break;
      case MiniboxConstants.BOOLEAN:
        ((MbArray_Z<?>)mbArray).update_J(index, value);
        break;
      case MiniboxConstants.UNIT:
        ((MbArray_V<?>)mbArray).update_J(index, value);
        break;
      default:
        mbArray.update(index, MiniboxConversionsLong.<T>minibox2box(value, T_Tag));
        break;
    }
  }

  public static <T> void mbArray_update_D(MbArray<T> mbArray, int index, double value, byte T_Tag) {
    if (mbArray instanceof MbArray_D<?>)
      ((MbArray_D<?>)mbArray).update_D(index, value);
    else
      mbArray.update(index, MiniboxConversionsDouble.<T>minibox2box(value, T_Tag));
  }

  @SuppressWarnings("unchecked")
  public static final <T> MbArray<T> mbArray_empty_J(int size, byte T_Tag) {
    switch(T_Tag) {
    case MiniboxConstants.LONG:
      return new MbArray_J<T>(size);
    case MiniboxConstants.INT:
      return new MbArray_I<T>(size);
    case MiniboxConstants.SHORT:
      return new MbArray_S<T>(size);
    case MiniboxConstants.CHAR:
      return new MbArray_C<T>(size);
    case MiniboxConstants.BYTE:
      return new MbArray_B<T>(size);
    case MiniboxConstants.BOOLEAN:
      return new MbArray_Z<T>(size);
    case MiniboxConstants.UNIT:
      return new MbArray_V<T>(size);
    default:
      return new MbArray_L<T>(size);
    }
  }

  @SuppressWarnings("unchecked")
  public static <T> MbArray<T> mbArray_empty_D(int size, byte T_Tag) {
    return new MbArray_D<T>(T_Tag, size);
  }

  @SuppressWarnings("unchecked")
  public static final <T> MbArray<T> mbArray_clone_J(Object array, byte T_Tag) {
    switch(T_Tag) {
    case MiniboxConstants.LONG:
      return new MbArray_J<T>(array);
    case MiniboxConstants.INT:
      return new MbArray_I<T>(array);
    case MiniboxConstants.SHORT:
      return new MbArray_S<T>(array);
    case MiniboxConstants.CHAR:
      return new MbArray_C<T>(array);
    case MiniboxConstants.BYTE:
      return new MbArray_B<T>(array);
    case MiniboxConstants.BOOLEAN:
      return new MbArray_Z<T>(array);
    case MiniboxConstants.UNIT:
      return new MbArray_V<T>(array);
    default:
      return new MbArray_L<T>(array);
    }
  }

  @SuppressWarnings("unchecked")
  public static <T> MbArray<T> mbArray_clone_D(Object array, byte T_Tag) {
    return new MbArray_D<T>(T_Tag, array);
  }
}