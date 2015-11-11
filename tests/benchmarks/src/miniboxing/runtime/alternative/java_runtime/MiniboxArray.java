package miniboxing.runtime.alternative.java_runtime;
import miniboxing.internal.MiniboxConstants;

public class MiniboxArray {

  public final static Object unitArray = new Object();

  public final static <T> Object mbarray_new(int len, byte tag) {
    if (tag >= MiniboxConstants.CHAR)
      return mbarray_new_1(len, tag);
    else
      return mbarray_new_2(len, tag);
  }

  private final static <T> Object mbarray_new_1(int len, byte tag) {
    switch (tag) {
      case MiniboxConstants.BOOLEAN:
        return new boolean[len];
      case MiniboxConstants.BYTE:
        return new byte[len];
      case MiniboxConstants.SHORT:
        return new short[len];
    }
    return unitArray;
  }

  private final static <T> Object mbarray_new_2(int len, byte tag) {
    switch (tag) {
      case MiniboxConstants.CHAR:
        return new char[len];
      case MiniboxConstants.INT:
        return new int[len];
      case MiniboxConstants.LONG:
        return new long[len];
    }
    return unitArray;
  }

////////////////////////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  public final static long mbarray_apply_minibox(Object array, int idx, byte tag) {
    if (tag >= MiniboxConstants.CHAR)
      return mbarray_apply_minibox_1(array, idx, tag);
    else
      return mbarray_apply_minibox_2(array, idx, tag);
  }

  private final static long mbarray_apply_minibox_1(Object array, int idx, byte tag) {
    switch (tag) {
      case MiniboxConstants.BOOLEAN:
        if (((boolean[])array)[idx])
          return 1;
        else
          return 0;
      case MiniboxConstants.BYTE:
        return ((byte[])array)[idx];
      case MiniboxConstants.SHORT:
        return ((short[])array)[idx];
    }
    return 0;
  }

  private final static long mbarray_apply_minibox_2(Object array, int idx, byte tag) {
    switch (tag) {
      case MiniboxConstants.CHAR:
        return ((char[])array)[idx];
      case MiniboxConstants.INT:
        return ((int[])array)[idx];
      case MiniboxConstants.LONG:
        return ((long[])array)[idx];
    }
    return 0;
  }

////////////////////////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  @SuppressWarnings("unchecked")
  public final static <T> T mbarray_apply_box(Object array, int idx, byte tag) {
    if (tag >= MiniboxConstants.CHAR)
      return (T)mbarray_apply_box_1(array, idx, tag);
    else
      return (T)mbarray_apply_box_2(array, idx, tag);
  }

  private final static Object mbarray_apply_box_1(Object array, int idx, byte tag) {
    switch (tag) {
    case MiniboxConstants.BOOLEAN:
      return ((boolean[])array)[idx];
    case MiniboxConstants.BYTE:
      return ((byte[])array)[idx];
    case MiniboxConstants.SHORT:
      return ((short[])array)[idx];
    }
    return 0;
  }

  private final static Object mbarray_apply_box_2(Object array, int idx, byte tag) {
    switch (tag) {
      case MiniboxConstants.CHAR:
        return ((char[])array)[idx];
      case MiniboxConstants.INT:
        return ((int[])array)[idx];
      case MiniboxConstants.LONG:
        return ((long[])array)[idx];
    }
    return 0;
  }

////////////////////////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  public final static void mbarray_update_minibox(Object array, int idx, long value, byte tag) {
    if (tag >= MiniboxConstants.CHAR)
      mbarray_update_minibox_1(array, idx, value, tag);
    else
      mbarray_update_minibox_2(array, idx, value, tag);
  }

  private final static void mbarray_update_minibox_1(Object array, int idx, long value, byte tag) {
    switch (tag) {
      case MiniboxConstants.BOOLEAN:
        if (value == 0)
          ((boolean[])array)[idx] = false;
        else
          ((boolean[])array)[idx] = true;
      case MiniboxConstants.BYTE:
        ((byte[])array)[idx] = (byte)value;
      case MiniboxConstants.SHORT:
        ((short[])array)[idx] = (short)value;
    }
  }

  private final static void mbarray_update_minibox_2(Object array, int idx, long value, byte tag) {
    switch (tag) {
      case MiniboxConstants.CHAR:
        ((char[])array)[idx] = (char)value;
      case MiniboxConstants.INT:
        ((int[])array)[idx] = (int)value;
      case MiniboxConstants.LONG:
        ((long[])array)[idx] = value;
    }
  }

////////////////////////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  public final static <T> void mbarray_update_box(Object array, int idx, T value, byte tag) {
    if (tag >= MiniboxConstants.CHAR)
      mbarray_update_box_1(array, idx, value, tag);
    else
      mbarray_update_box_2(array, idx, value, tag);
  }

  private final static void mbarray_update_box_1(Object array, int idx, Object value, byte tag) {
    switch (tag) {
      case MiniboxConstants.BOOLEAN:
        ((boolean[])array)[idx] = (java.lang.Boolean)value;
      case MiniboxConstants.BYTE:
        ((byte[])array)[idx] = (java.lang.Byte)value;
      case MiniboxConstants.SHORT:
        ((short[])array)[idx] = (java.lang.Short)value;
    }
  }

  private final static void mbarray_update_box_2(Object array, int idx, Object value, byte tag) {
    switch (tag) {
      case MiniboxConstants.CHAR:
        ((char[])array)[idx] = (java.lang.Character)value;
      case MiniboxConstants.INT:
        ((int[])array)[idx] = (java.lang.Integer)value;
      case MiniboxConstants.LONG:
        ((long[])array)[idx] = (java.lang.Long)value;
    }
  }

////////////////////////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  public final static int mbarray_length(Object array, byte tag) {
    if (tag >= MiniboxConstants.CHAR)
      return mbarray_length_1(array, tag);
    else
      return mbarray_length_2(array, tag);
  }

  private final static int mbarray_length_1(Object array, byte tag) {
    switch (tag) {
      case MiniboxConstants.BOOLEAN:
        return ((boolean[])array).length;
      case MiniboxConstants.BYTE:
        return ((byte[])array).length;
      case MiniboxConstants.SHORT:
        return ((short[])array).length;
    }
    return 0;
  }

  private final static int mbarray_length_2(Object array, byte tag) {
    switch (tag) {
      case MiniboxConstants.CHAR:
        return ((char[])array).length;
      case MiniboxConstants.INT:
        return ((int[])array).length;
      case MiniboxConstants.LONG:
        return ((long[])array).length;
    }
    return 0;
  }
}

