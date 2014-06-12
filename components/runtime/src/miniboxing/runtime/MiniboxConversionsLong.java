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


public class MiniboxConversionsLong {

  @SuppressWarnings("unchecked")
  public final static <T> T minibox2box(long l, byte tag) {
//  val i: Int = null
//  println(i) // produces 0
//
//    if (l == Long.MIN_VALUE)
//      return null;
//    else
//      return (T)minibox2box_deep(l, tag);
    return (T)minibox2box_deep(l, tag);
  }

  private final static Object minibox2box_deep(long l, byte tag) {
    switch(tag) {
      case MiniboxConstants.CHAR:
        return (char)l;
      case MiniboxConstants.INT:
        return (int)l;
      default:
        return l;
    }
  }

  public final static <T> long box2minibox_tt(T a, byte tag) {
    if (a == null)
//      return Long.MIN_VALUE;
      return 0;
    else
      return box2minibox_deep(a, tag);
  }

  public final static <T> long box2minibox_deep(T a, byte tag) {
    switch(tag) {
      case MiniboxConstants.CHAR:
        return (java.lang.Character)a;
      case MiniboxConstants.INT:
        return (java.lang.Integer)a;
      default:
        return (java.lang.Long)a;
    }
  }
}
