package miniboxing.runtime;

import miniboxing.runtime.MiniboxConstants;


public class MiniboxConversions {

  public final static scala.runtime.BoxedUnit minibox2unit(long l) {
    return scala.runtime.BoxedUnit.UNIT;
  }

  public final static boolean minibox2boolean(long l) {
    return (l != 0);
  }

  public final static byte minibox2byte(long l) {
    return (byte)l;
  }

  public final static char minibox2char(long l) {
    return (char)l;
  }

  public final static short minibox2short(long l) {
    return (short)l;
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

  public final static long unit2minibox(scala.runtime.BoxedUnit u) {
    return 0;
  }

  public final static long boolean2minibox(boolean b) {
    return b?1:0;
  }

  public final static long byte2minibox(byte b) {
    return b;
  }

  public final static long char2minibox(char c) {
    return c;
  }

  public final static long short2minibox(short s) {
    return s;
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
    return (T)minibox2box_deep(l, tag);
  }

  private final static Object minibox2box_deep(long l, byte tag) {
    switch(tag) {
      case MiniboxConstants.UNIT:
        return scala.runtime.BoxedUnit.UNIT;
      case MiniboxConstants.BOOLEAN:
        return (l != 0)?true:false;
      case MiniboxConstants.BYTE:
        return (byte)l;
      case MiniboxConstants.SHORT:
        return (short)l;
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

    /*
   *  Used in the rewiring, to keep the type and tag on all types
   */
  public final static <T> long box2minibox_tt(T a, byte tag) {
    switch(tag) {
      case MiniboxConstants.UNIT:
        return 0;
      case MiniboxConstants.BOOLEAN:
        return ((java.lang.Boolean)a)?1:0;
      case MiniboxConstants.BYTE:
        return (java.lang.Byte)a;
      case MiniboxConstants.SHORT:
        return (java.lang.Short)a;
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
}
