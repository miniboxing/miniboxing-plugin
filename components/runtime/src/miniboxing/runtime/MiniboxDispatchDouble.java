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

public class MiniboxDispatchDouble {

  public final static String mboxed_toString(double x, byte tag) {
    switch(tag) {
      case MiniboxConstants.FLOAT:
        return Float.valueOf((float)x).toString();
      default:
        return Double.valueOf(x).toString();
    }
  }

  public final static boolean mboxed_eqeq(double x, byte xtag, Object other) {
    return MiniboxConversionsDouble.minibox2box(x, xtag).equals(other);
  }

  public final static boolean mboxed_eqeq(double x, byte xtag, double y, byte ytag) {
    if (xtag == ytag)
      return (x == y);
    else
      return MiniboxConversionsDouble.minibox2box(x, xtag).equals(MiniboxConversionsDouble.minibox2box(y, ytag));
  }

  public final static boolean mboxed_eqeq(double x, double y) {
    return (x == y);
  }

  // non-overloaded:
  public final static boolean mboxed_eqeq_other(double x, byte xtag, Object other) {
    return mboxed_eqeq(x, xtag, other);
  }

  // non-overloaded:
  public final static boolean mboxed_eqeq_tag(double x, byte xtag, double y, byte ytag) {
    return mboxed_eqeq(x, xtag, y, ytag);
  }

  // non-overloaded:
  public final static boolean mboxed_eqeq_notag(double x, double y) {
    return mboxed_eqeq(x, y);
  }

  public final static int mboxed_hashCode(double x, byte tag) {
    switch(tag) {
      case MiniboxConstants.FLOAT:
        return Float.valueOf((float)x).hashCode();
      default:
        return Double.valueOf(x).hashCode();
    }
  }
}
