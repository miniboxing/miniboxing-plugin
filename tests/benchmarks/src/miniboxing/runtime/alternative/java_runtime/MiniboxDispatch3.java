package miniboxing.runtime.alternative.java_runtime;

import miniboxing.internal.MiniboxConstants;

public class MiniboxDispatch3 {

  public final static String mboxed_toString(long x, byte tag) {
    switch(tag) {
      case MiniboxConstants.UNIT:
        return scala.runtime.BoxedUnit.UNIT.toString();
      case MiniboxConstants.BOOLEAN:
        return Boolean.valueOf(x != 0).toString();
      case MiniboxConstants.BYTE:
        return Byte.valueOf((byte)x).toString();
      case MiniboxConstants.SHORT:
        return Short.valueOf((short)x).toString();
      case MiniboxConstants.CHAR:
        return Character.valueOf((char)x).toString();
      case MiniboxConstants.INT:
        return Integer.valueOf((int)x).toString();
      default:
        return Long.valueOf(x).toString();
    }
  }

  public final static boolean mboxed_eqeq(long x, byte xtag, Object other) {
    return MiniboxConversions3.minibox2box(x, xtag).equals(other);
  }

  public final static boolean mboxed_eqeq(long x, byte xtag, long y, byte ytag) {
    if (xtag == ytag)
      return (x == y);
    else
      return MiniboxConversions3.minibox2box(x, xtag).equals(MiniboxConversions3.minibox2box(y, ytag));
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
      case MiniboxConstants.UNIT:
        return scala.runtime.BoxedUnit.UNIT.hashCode();
      case MiniboxConstants.BOOLEAN:
        return Boolean.valueOf(x != 0).hashCode();
      case MiniboxConstants.BYTE:
        return Byte.valueOf((byte)x).hashCode();
      case MiniboxConstants.SHORT:
        return Short.valueOf((short)x).hashCode();
      case MiniboxConstants.CHAR:
        return Character.valueOf((char)x).hashCode();
      case MiniboxConstants.INT:
        return Integer.valueOf((int)x).hashCode();
      default:
        return Long.valueOf(x).hashCode();
    }
  }
}
