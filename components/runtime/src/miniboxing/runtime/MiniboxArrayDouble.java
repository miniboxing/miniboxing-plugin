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

public class MiniboxArrayDouble {

  public final static double mbarray_apply_minibox(Object array, int idx, byte tag) {
    switch(tag) {
      case MiniboxConstants.FLOAT:
        return ((float[])array)[idx];
      default:
        return ((double[])array)[idx];
    }
  }

////////////////////////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  public final static void mbarray_update_minibox(Object array, int idx, double value, byte tag) {
    switch(tag) {
      case MiniboxConstants.FLOAT:
        ((float[])array)[idx] = (float)value;
        return;
      default:
        ((double[])array)[idx] = value;
        return;
    }
  }
}

