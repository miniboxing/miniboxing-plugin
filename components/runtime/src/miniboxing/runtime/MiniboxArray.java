//
//     _____   .__         .__ ___.                    .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_ |__    ____  ___  ___|__|  ____     ____
//   /  \ /  \ |  | /    \ |  | | __ \  /  _ \ \  \/  /|  | /    \   / ___\
//  /    Y    \|  ||   |  \|  | | \_\ \(  <_> ) >    < |  ||   |  \ / /_/  >
//  \____|__  /|__||___|  /|__| |___  / \____/ /__/\_ \|__||___|  / \___  /
//          \/          \/          \/               \/         \/ /_____/
// Copyright (c) 2012-2014 Scala Team, École polytechnique fédérale de Lausanne
//
// Authors:
//    * Vlad Ureche
//
package miniboxing.runtime;
import miniboxing.runtime.MiniboxConstants;

public class MiniboxArray {

  public final static Object unitArray = new Object();

  public final static <T> Object mbarray_new(int len, byte tag) {
    switch(tag) {
      case MiniboxConstants.UNIT:
        return new scala.runtime.BoxedUnit[len];
      case MiniboxConstants.BOOLEAN:
        return new boolean[len];
      case MiniboxConstants.BYTE:
        return new byte[len];
      case MiniboxConstants.SHORT:
        return new short[len];
      case MiniboxConstants.CHAR:
        return new char[len];
      case MiniboxConstants.INT:
        return new int[len];
      case MiniboxConstants.FLOAT:
        return new float[len];
      case MiniboxConstants.DOUBLE:
        return new double[len];
      default:
        return new long[len];
    }
  }

////////////////////////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


  public final static long mbarray_apply_minibox(Object array, int idx, byte tag) {
    switch(tag) {
      case MiniboxConstants.UNIT:
        return 0l;
      case MiniboxConstants.BOOLEAN:
        return ((boolean[])array)[idx]?1:0;
      case MiniboxConstants.BYTE:
        return ((byte[])array)[idx];
      case MiniboxConstants.SHORT:
        return ((short[])array)[idx];
      case MiniboxConstants.CHAR:
        return ((char[])array)[idx];
      case MiniboxConstants.INT:
        return ((int[])array)[idx];
      case MiniboxConstants.FLOAT:
        return Float.floatToIntBits(((float[])array)[idx]);
      case MiniboxConstants.DOUBLE:
        return Double.doubleToLongBits(((double[])array)[idx]);
      default:
        return ((long[])array)[idx];
    }
  }

////////////////////////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  public final static Object mbarray_apply_box(Object array, int idx, byte tag) {
    switch(tag) {
      case MiniboxConstants.UNIT:
        return ((scala.runtime.BoxedUnit[])array)[idx];
      case MiniboxConstants.BOOLEAN:
        return ((boolean[])array)[idx]?1:0;
      case MiniboxConstants.BYTE:
        return ((byte[])array)[idx];
      case MiniboxConstants.SHORT:
        return ((short[])array)[idx];
      case MiniboxConstants.CHAR:
        return ((char[])array)[idx];
      case MiniboxConstants.INT:
        return ((int[])array)[idx];
      case MiniboxConstants.FLOAT:
        return Float.floatToIntBits(((float[])array)[idx]);
      case MiniboxConstants.DOUBLE:
        return Double.doubleToLongBits(((double[])array)[idx]);
      default:
        return ((long[])array)[idx];
    }
  }

////////////////////////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  public final static void mbarray_update_minibox(Object array, int idx, long value, byte tag) {
    switch(tag) {
      case MiniboxConstants.UNIT:
        ((scala.runtime.BoxedUnit[])array)[idx] = scala.runtime.BoxedUnit.UNIT;
        return;
      case MiniboxConstants.BOOLEAN:
        ((boolean[])array)[idx] = (value == 0) ? false:true;
        return;
      case MiniboxConstants.BYTE:
        ((byte[])array)[idx] = (byte)value;
        return;
      case MiniboxConstants.SHORT:
        ((short[])array)[idx] = (short)value;
        return;
      case MiniboxConstants.CHAR:
        ((char[])array)[idx] = (char)value;
        return;
      case MiniboxConstants.INT:
        ((int[])array)[idx] = (int)value;
        return;
      case MiniboxConstants.FLOAT:
        ((float[])array)[idx] = Float.intBitsToFloat((int)value);
        return;
      case MiniboxConstants.DOUBLE:
        ((double[])array)[idx] = Double.longBitsToDouble(value);
        return;
      default:
        ((long[])array)[idx] = value;
        return;
    }
  }

////////////////////////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  public final static void mbarray_update_box(Object array, int idx, Object value, byte tag) {
    switch(tag) {
      case MiniboxConstants.UNIT:
        ((scala.runtime.BoxedUnit[])array)[idx] = (scala.runtime.BoxedUnit)value;
        return;
      case MiniboxConstants.BOOLEAN:
        ((boolean[])array)[idx] = (java.lang.Boolean)value;
        return;
      case MiniboxConstants.BYTE:
        ((byte[])array)[idx] = (java.lang.Byte)value;
        return;
      case MiniboxConstants.SHORT:
        ((short[])array)[idx] = (java.lang.Short)value;
        return;
      case MiniboxConstants.CHAR:
        ((char[])array)[idx] = (java.lang.Character)value;
        return;
      case MiniboxConstants.INT:
        ((int[])array)[idx] = (java.lang.Integer)value;
        return;
      case MiniboxConstants.FLOAT:
        ((float[])array)[idx] = (java.lang.Float)value;
        return;
      case MiniboxConstants.DOUBLE:
        ((double[])array)[idx] = (java.lang.Double)value;
        return;
      default:
        ((long[])array)[idx] = (java.lang.Long)value;
        return;
    }
  }

////////////////////////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  public final static int mbarray_length(Object array, byte tag) {
    switch(tag) {
      case MiniboxConstants.UNIT:
        return ((scala.runtime.BoxedUnit[])array).length;
      case MiniboxConstants.BOOLEAN:
        return ((boolean[])array).length;
      case MiniboxConstants.BYTE:
        return ((byte[])array).length;
      case MiniboxConstants.SHORT:
        return ((short[])array).length;
      case MiniboxConstants.CHAR:
        return ((char[])array).length;
      case MiniboxConstants.INT:
        return ((int[])array).length;
      case MiniboxConstants.FLOAT:
        return ((float[])array).length;
      case MiniboxConstants.DOUBLE:
        return ((double[])array).length;
      default:
        return ((long[])array).length;
    }
  }
}

