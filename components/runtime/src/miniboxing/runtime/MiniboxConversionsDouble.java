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


public class MiniboxConversionsDouble {

  public final static float minibox2float(double l) {
    return (float)l;
  }

  public final static double minibox2double(double l) {
    return l;
  }

  public final static double float2minibox(float f) {
    return f;
  }

  public final static double double2minibox(double f) {
    return f;
  }

  @SuppressWarnings("unchecked")
  public final static <T> T minibox2box(double l, byte tag) {
    return (T)minibox2box_deep(l, tag);
  }

  private final static Object minibox2box_deep(double l, byte tag) {
    switch(tag) {
      case MiniboxConstants.FLOAT:
        return (float)l;
      default:
        return l;
    }
  }

  public final static <T> double box2minibox_tt(T a, byte tag) {
    switch(tag) {
      case MiniboxConstants.FLOAT:
        return (java.lang.Float)a;
      default:
        return (java.lang.Double)a;
    }
  }

}
