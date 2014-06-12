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


public class MiniboxConversions {

  public final static char minibox2char(long l) {
    return (char)l;
  }

  public final static int minibox2int(long l) {
    return (int)l;
  }

  public final static long minibox2long(long l) {
    return l;
  }

  public final static float minibox2float(long l) {
    return Float.intBitsToFloat((int)l);
  }

  public final static double minibox2double(long l) {
    return Double.longBitsToDouble(l);
  }

  public final static long char2minibox(char c) {
    return c;
  }

  public final static long int2minibox(int i) {
    return i;
  }

  public final static long long2minibox(long l) {
    return l;
  }

  public final static long float2minibox(float f) {
    return (long) Float.floatToIntBits(f);
  }

  public final static long double2minibox(double f) {
    return (long) Double.doubleToLongBits(f);
  }

  /**
   * If code like the one below:
   *  class A[T](t: T) {
   *    override def toString = "A" + t
   *  }
   * in the specializer, we have to insert a box operation which takes into
   * account the type-tag of 't'. Normally, such an operation has to be
   * inserted during erasure, but we don't want to touch it.
   *
   * As a workaround, in our test examples we manually insert minibox2box
   * in such places.
   */
  @SuppressWarnings("unchecked")
  public final static <T> T minibox2box(long l, byte tag) {
// NOTE: We can't treat "null" correctly anyway, due to the scalac numeric conversions
//  val i: Int = null
//  println(i) // produces 0
//
//    if (l == Long.MIN_VALUE)
//      return null;
//    else
//    return (T)minibox2box_deep(l, tag);
    return (T)minibox2box_deep(l, tag);
  }

  private final static Object minibox2box_deep(long l, byte tag) {
    switch(tag) {
      case MiniboxConstants.CHAR:
        return (char)l;
      case MiniboxConstants.INT:
        return (int)l;
      case MiniboxConstants.FLOAT:
        return (float)Float.intBitsToFloat((int) l);
      case MiniboxConstants.DOUBLE:
        return (double)Double.longBitsToDouble(l);
      default:
        return l;
    }
  }

  // TODO: This needs to be removed, it's here just for
  // some stupid old benchmarks nobody cares about.
  public final static long box2minibox(Object a) {
    return 0;
  }

  public final static <T> long box2minibox_tt(T a, byte tag) {
    if (a == null)
//      return Long.MIN_VALUE;
      return 0l;
    else
      return box2minibox_deep(a, tag);
  }

  /*
   *  Used in the rewiring, to keep the type and tag on all types
   */
  public final static <T> long box2minibox_deep(T a, byte tag) {
    switch(tag) {
      case MiniboxConstants.CHAR:
        return (java.lang.Character)a;
      case MiniboxConstants.INT:
        return (java.lang.Integer)a;
      case MiniboxConstants.FLOAT:
        return Float.floatToIntBits((java.lang.Float)a);
      case MiniboxConstants.DOUBLE:
        return Double.doubleToLongBits((java.lang.Double)a);
      default:
        return (java.lang.Long)a;
    }
  }

  public final static <T> T unreachableConversion(String repr1, String repr2) {
    throw new Error("Unreachable conversion from " + repr1 + " to " + repr2 + ". Did you cast? :p");
  }
}
