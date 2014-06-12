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

public class MiniboxDispatchLong {

  public final static String mboxed_toString(long x, byte tag) {
    switch(tag) {
      case MiniboxConstants.CHAR:
        return Character.valueOf((char)x).toString();
      case MiniboxConstants.INT:
        return Integer.valueOf((int)x).toString();
      default:
        return Long.valueOf(x).toString();
    }
  }

  public final static boolean mboxed_eqeq(long x, byte xtag, Object other) {
    return MiniboxConversionsLong.minibox2box(x, xtag).equals(other);
  }

  public final static boolean mboxed_eqeq(long x, byte xtag, long y, byte ytag) {
    if (xtag == ytag)
      return (x == y);
    else
      return MiniboxConversionsLong.minibox2box(x, xtag).equals(MiniboxConversionsLong.minibox2box(y, ytag));
  }

  public final static boolean mboxed_eqeq(long x, long y) {
    return (x == y);
  }

  // non-overloaded:
  public final static  boolean mboxed_eqeq_other(long x, byte xtag, Object other) {
    return mboxed_eqeq(x, xtag, other);
  }

  // non-overloaded:
  public final static  boolean mboxed_eqeq_tag(long x, byte xtag, long y, byte ytag) {
    return mboxed_eqeq(x, xtag, y, ytag);
  }

  // non-overloaded:
  public final static  boolean mboxed_eqeq_notag(long x, long y) {
    return mboxed_eqeq(x, y);
  }

  public final static int mboxed_hashCode(long x, byte tag) {
    switch(tag) {
      case MiniboxConstants.CHAR:
        return Character.valueOf((char)x).hashCode();
      case MiniboxConstants.INT:
        return Integer.valueOf((int)x).hashCode();
      default:
        return Long.valueOf(x).hashCode();
    }
  }
}
