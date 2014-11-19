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

public class MiniboxArrayLong {

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
      default:
        ((long[])array)[idx] = value;
        return;
    }
  }
}

