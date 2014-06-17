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
      default:
        ((long[])array)[idx] = value;
        return;
    }
  }

////////////////////////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  public final static int mbarray_length(Object array, byte tag) {
    switch(tag) {
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

