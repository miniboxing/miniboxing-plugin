package miniboxing.runtime.array;

import miniboxing.runtime.MiniboxConstants;
import miniboxing.runtime.MiniboxConversionsDouble;
import miniboxing.runtime.MiniboxConversionsLong;
import scala.MbArray;
import scala.collection.mutable.WrappedArray;

public class MbArrayOpts {

  public static final <T> long mbArray_apply_J(MbArray<T> mbArray, int index, byte T_Tag) {
    if (mbArray instanceof MbArray_J<?>)
      return ((MbArray_J<?>)mbArray).apply_J(index);
    else if (mbArray instanceof MbArray_I<?>)
      return ((MbArray_I<?>)mbArray).apply_J(index);
    else if (mbArray instanceof MbArray_S<?>)
      return ((MbArray_S<?>)mbArray).apply_J(index);
    else if (mbArray instanceof MbArray_C<?>)
      return ((MbArray_C<?>)mbArray).apply_J(index);
    else if (mbArray instanceof MbArray_B<?>)
      return ((MbArray_B<?>)mbArray).apply_J(index);
    else if (mbArray instanceof MbArray_Z<?>)
      return ((MbArray_Z<?>)mbArray).apply_J(index);
    else if (mbArray instanceof MbArray_V<?>)
      return ((MbArray_V<?>)mbArray).apply_J(index);
    else
      return MiniboxConversionsLong.<T>box2minibox_tt(mbArray.apply(index), T_Tag);

//
//  This is slower, at least for the Optimistic Respecialization benchmark:
//
//    switch(T_Tag) {
//      case MiniboxConstants.LONG:
//        return ((MbArray_J<?>)mbArray).apply_J(index);
//      case MiniboxConstants.INT:
//        return ((MbArray_I<?>)mbArray).apply_J(index);
//      case MiniboxConstants.SHORT:
//        return ((MbArray_S<?>)mbArray).apply_J(index);
//      case MiniboxConstants.CHAR:
//        return ((MbArray_C<?>)mbArray).apply_J(index);
//      case MiniboxConstants.BYTE:
//        return ((MbArray_B<?>)mbArray).apply_J(index);
//      case MiniboxConstants.BOOLEAN:
//        return ((MbArray_Z<?>)mbArray).apply_J(index);
//      case MiniboxConstants.UNIT:
//        return ((MbArray_V<?>)mbArray).apply_J(index);
//      default:
//        return MiniboxConversionsLong.<T>box2minibox_tt(mbArray.apply(index), T_Tag);
//    }
  }

  public static final <T> double mbArray_apply_D(MbArray<T> mbArray, int index, byte T_Tag) {
    if (mbArray instanceof MbArray_F<?>)
      return ((MbArray_F<?>)mbArray).apply_D(index);
    else if (mbArray instanceof MbArray_D<?>)
      return ((MbArray_D<?>)mbArray).apply_D(index);
    else
      return MiniboxConversionsDouble.box2minibox_tt(mbArray.apply(index), T_Tag);
//
//  This is slower, at least for the Optimistic Respecialization benchmark:
//
//    switch(T_Tag) {
//      case MiniboxConstants.DOUBLE:
//      	return ((MbArray_D<?>)mbArray).apply_D(index);
//      case MiniboxConstants.FLOAT:
//      	return ((MbArray_F<?>)mbArray).apply_D(index);
//      default:
//      	return MiniboxConversionsDouble.box2minibox_tt(mbArray.apply(index), T_Tag);
//    }
  }

  public static final <T> void mbArray_update_J(MbArray<T> mbArray, int index, long value, byte T_Tag) {
    if (mbArray instanceof MbArray_J<?>)
      ((MbArray_J<?>)mbArray).update_J(index, value);
    else if (mbArray instanceof MbArray_I<?>)
      ((MbArray_I<?>)mbArray).update_J(index, value);
    else if (mbArray instanceof MbArray_S<?>)
      ((MbArray_S<?>)mbArray).update_J(index, value);
    else if (mbArray instanceof MbArray_C<?>)
      ((MbArray_C<?>)mbArray).update_J(index, value);
    else if (mbArray instanceof MbArray_B<?>)
      ((MbArray_B<?>)mbArray).update_J(index, value);
    else if (mbArray instanceof MbArray_Z<?>)
      ((MbArray_Z<?>)mbArray).update_J(index, value);
    else if (mbArray instanceof MbArray_V<?>)
      ((MbArray_V<?>)mbArray).update_J(index, value);
    else
      mbArray.update(index, MiniboxConversionsLong.<T>minibox2box(value, T_Tag));
//
//  This is slower, at least for the Optimistic Respecialization benchmark:
//
//    switch(T_Tag) {
//      case MiniboxConstants.LONG:
//        ((MbArray_J<?>)mbArray).update_J(index, value);
//        break;
//      case MiniboxConstants.INT:
//        ((MbArray_I<?>)mbArray).update_J(index, value);
//        break;
//      case MiniboxConstants.SHORT:
//        ((MbArray_S<?>)mbArray).update_J(index, value);
//        break;
//      case MiniboxConstants.CHAR:
//        ((MbArray_C<?>)mbArray).update_J(index, value);
//        break;
//      case MiniboxConstants.BYTE:
//        ((MbArray_B<?>)mbArray).update_J(index, value);
//        break;
//      case MiniboxConstants.BOOLEAN:
//        ((MbArray_Z<?>)mbArray).update_J(index, value);
//        break;
//      case MiniboxConstants.UNIT:
//        ((MbArray_V<?>)mbArray).update_J(index, value);
//        break;
//      default:
//        mbArray.update(index, MiniboxConversionsLong.<T>minibox2box(value, T_Tag));
//        break;
//    }
  }

  public static final <T> void mbArray_update_D(MbArray<T> mbArray, int index, double value, byte T_Tag) {
    if (mbArray instanceof MbArray_F<?>)
      ((MbArray_F<?>)mbArray).update_D(index, value);
    if (mbArray instanceof MbArray_D<?>)
      ((MbArray_D<?>)mbArray).update_D(index, value);
    else
      mbArray.update(index, MiniboxConversionsDouble.<T>minibox2box(value, T_Tag));
//
//  This is slower, at least for the Optimistic Respecialization benchmark:
//
//	switch(T_Tag) {
//  	case MiniboxConstants.DOUBLE:
//  		((MbArray_D<?>)mbArray).update_D(index, value);
//  		break;
//  	case MiniboxConstants.FLOAT:
//  		((MbArray_F<?>)mbArray).update_D(index, value);
//  		break;
//  	default:
//  		mbArray.update(index, MiniboxConversionsDouble.<T>minibox2box(value, T_Tag));
//	}
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
  public static final <T> MbArray<T> mbArray_empty_D(int size, byte T_Tag) {
    switch(T_Tag){
      case MiniboxConstants.DOUBLE:
        return new MbArray_D<T>(size);
      case MiniboxConstants.FLOAT:
        return new MbArray_F<T>(size);
      default:
        return new MbArray_L<T>(size);
    }
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
  public static final <T> MbArray<T> mbArray_clone_D(Object array, byte T_Tag) {
    switch(T_Tag){
      case MiniboxConstants.DOUBLE:
        return new MbArray_D<T>(array);
      case MiniboxConstants.FLOAT:
        return new MbArray_F<T>(array);
      default:
        return new MbArray_L<T>(array);
    }
  }

  @SuppressWarnings("unchecked")
  public static final <T> MbArray<T> mbArray_apply_constr_J(Object array, byte T_Tag) {
	switch(T_Tag) {
      case MiniboxConstants.LONG:
        return new MbArray_J<T>((WrappedArray)array);
      case MiniboxConstants.INT:
        return new MbArray_I<T>((WrappedArray)array);
      case MiniboxConstants.SHORT:
        return new MbArray_S<T>((WrappedArray)array);
      case MiniboxConstants.CHAR:
        return new MbArray_C<T>((WrappedArray)array);
      case MiniboxConstants.BYTE:
        return new MbArray_B<T>((WrappedArray)array);
      case MiniboxConstants.BOOLEAN:
        return new MbArray_Z<T>((WrappedArray)array);
      case MiniboxConstants.UNIT:
        return new MbArray_V<T>((WrappedArray)array);
      default:
        return new MbArray_L<T>((WrappedArray)array);
    }
  }

  @SuppressWarnings("unchecked")
  public static final <T> MbArray<T> mbArray_apply_constr_D(Object array, byte T_Tag) {
	switch(T_Tag){
      case MiniboxConstants.DOUBLE:
        return new MbArray_D<T>((WrappedArray)array);
      case MiniboxConstants.FLOAT:
        return new MbArray_F<T>((WrappedArray)array);
      default:
        return new MbArray_L<T>((WrappedArray)array);
    }
  }

  @SuppressWarnings("unchecked")
  public static final <T> MbArray<T> mbArray_apply_constr_prim_J(Object array, byte T_Tag) {
	return mbArray_clone_J(((WrappedArray)array).array(), T_Tag);
  }

  @SuppressWarnings("unchecked")
  public static final <T> MbArray<T> mbArray_apply_constr_prim_D(Object array, byte T_Tag) {
	return mbArray_clone_D(((WrappedArray)array).array(), T_Tag);
  }
}