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
      case MiniboxConstants.CHAR:
        return ((char[])array)[idx];
      case MiniboxConstants.INT:
        return ((int[])array)[idx];
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
      default:
        ((long[])array)[idx] = value;
        return;
    }
  }
}

