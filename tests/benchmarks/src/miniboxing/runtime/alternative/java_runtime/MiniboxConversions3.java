package miniboxing.runtime.alternative.java_runtime;

import miniboxing.internal.MiniboxConstants;


public class MiniboxConversions3 {

  public final static scala.runtime.BoxedUnit MiniboxToUnit(long l) {
    return scala.runtime.BoxedUnit.UNIT;
  }

  public final static boolean MiniboxToBoolean(long l) {
    return (l != 0);
  }

  public final static byte MiniboxToByte(long l) {
    return (byte)l;
  }

  public final static char MiniboxToChar(long l) {
    return (char)l;
  }

  public final static short MiniboxToShort(long l) {
    return (short)l;
  }

  public final static int MiniboxToInt(long l) {
    return (int)l;
  }

  public final static long MiniboxToLong(long l) {
    return l;
  }



  public final static long UnitToMinibox(scala.runtime.BoxedUnit u) {
    return 0;
  }

  public final static long BooleanToMinibox(boolean b) {
    return b?1:0;
  }

  public final static long ByteToMinibox(byte b) {
    return b;
  }

  public final static long CharToMinibox(char c) {
    return c;
  }

  public final static long ShortToMinibox(short s) {
    return s;
  }

  public final static long IntToMinibox(int i) {
    return i;
  }

  public final static long LongToMinibox(long l) {
    return l;
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
      default:
        return l;
    }
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
      default:
        return (java.lang.Long)a;
    }
  }
}
