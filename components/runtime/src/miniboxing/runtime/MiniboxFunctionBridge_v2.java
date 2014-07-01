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
import miniboxing.runtime.MiniboxConversions;
import scala.Function0;
import scala.Function1;
import scala.Function2;

public class MiniboxFunctionBridge_v2 {

  final static byte MULTIPLIER = 10;

  public static <R> long function0_apply_long(byte R_Tag, Function0<R> f) {
    switch (R_Tag) {
    case MiniboxConstants.INT:
      return MiniboxConversions.int2minibox(f.apply$mcI$sp());
    case MiniboxConstants.LONG:
      return MiniboxConversions.long2minibox(f.apply$mcJ$sp());
    case MiniboxConstants.FLOAT:
      return MiniboxConversions.float2minibox(f.apply$mcF$sp());
    case MiniboxConstants.DOUBLE:
      return MiniboxConversions.double2minibox(f.apply$mcD$sp());
    default:
      return MiniboxConversions.box2minibox_tt(f.apply(), R_Tag);
    }
  }

  @SuppressWarnings("unchecked")
  public static <T, R> long function1_apply_long_long(byte T_Tag, byte R_Tag, Function1<T, R> f, long arg) {
    switch (R_Tag) {
    case MiniboxConstants.INT:
      return MiniboxConversions.int2minibox(function1_apply_long_long_int(T_Tag, f, arg));
    case MiniboxConstants.LONG:
      return MiniboxConversions.long2minibox(function1_apply_long_long_long(T_Tag, f, arg));
    case MiniboxConstants.FLOAT:
      return MiniboxConversions.float2minibox(function1_apply_long_long_float(T_Tag, f, arg));
    case MiniboxConstants.DOUBLE:
      return MiniboxConversions.double2minibox(function1_apply_long_long_double(T_Tag, f, arg));
    default:
      return MiniboxConversions.box2minibox_tt(f.apply((T)MiniboxConversions.minibox2box(arg, T_Tag)), R_Tag);
    }
  }

  @SuppressWarnings("unchecked")
  private static <T, R> int function1_apply_long_long_int(byte T_Tag, Function1<T, R> f, long arg) {
    switch (T_Tag) {
    case MiniboxConstants.INT:
      return f.apply$mcII$sp(MiniboxConversions.minibox2int(arg));
    case MiniboxConstants.LONG:
      return f.apply$mcIJ$sp(MiniboxConversions.minibox2long(arg));
    case MiniboxConstants.FLOAT:
      return f.apply$mcIF$sp(MiniboxConversions.minibox2float(arg));
    case MiniboxConstants.DOUBLE:
      return f.apply$mcID$sp(MiniboxConversions.minibox2double(arg));
    default:
      return ((Integer)(f.apply((T)MiniboxConversions.minibox2box(arg, T_Tag)))).intValue();
    }
  }

  @SuppressWarnings("unchecked")
  private static <T, R> long function1_apply_long_long_long(byte T_Tag, Function1<T, R> f, long arg) {
    switch (T_Tag) {
    case MiniboxConstants.INT:
      return f.apply$mcJI$sp(MiniboxConversions.minibox2int(arg));
    case MiniboxConstants.LONG:
      return f.apply$mcJJ$sp(MiniboxConversions.minibox2long(arg));
    case MiniboxConstants.FLOAT:
      return f.apply$mcJF$sp(MiniboxConversions.minibox2float(arg));
    case MiniboxConstants.DOUBLE:
      return f.apply$mcJD$sp(MiniboxConversions.minibox2double(arg));
    default:
      return ((Long)(f.apply((T)MiniboxConversions.minibox2box(arg, T_Tag)))).longValue();
    }
  }

  @SuppressWarnings("unchecked")
  private static <T, R> float function1_apply_long_long_float(byte T_Tag, Function1<T, R> f, long arg) {
    switch (T_Tag) {
    case MiniboxConstants.INT:
      return f.apply$mcFI$sp(MiniboxConversions.minibox2int(arg));
    case MiniboxConstants.LONG:
      return f.apply$mcFJ$sp(MiniboxConversions.minibox2long(arg));
    case MiniboxConstants.FLOAT:
      return f.apply$mcFF$sp(MiniboxConversions.minibox2float(arg));
    case MiniboxConstants.DOUBLE:
      return f.apply$mcFD$sp(MiniboxConversions.minibox2double(arg));
    default:
      return ((Float)(f.apply((T)MiniboxConversions.minibox2box(arg, T_Tag)))).floatValue();
    }
  }

  @SuppressWarnings("unchecked")
  private static <T, R> double function1_apply_long_long_double(byte T_Tag, Function1<T, R> f, long arg) {
    switch (T_Tag) {
    case MiniboxConstants.INT:
      return f.apply$mcDI$sp(MiniboxConversions.minibox2int(arg));
    case MiniboxConstants.LONG:
      return f.apply$mcDJ$sp(MiniboxConversions.minibox2long(arg));
    case MiniboxConstants.FLOAT:
      return f.apply$mcDF$sp(MiniboxConversions.minibox2float(arg));
    case MiniboxConstants.DOUBLE:
      return f.apply$mcDD$sp(MiniboxConversions.minibox2double(arg));
    default:
      return ((Double)(f.apply((T)MiniboxConversions.minibox2box(arg, T_Tag)))).doubleValue();
    }
  }

  @SuppressWarnings("unchecked")
  public static <T1, T2, R> long function2_apply_long_long_long(byte T1_Tag, byte T2_Tag, byte R_Tag, Function2<T1, T2, R> f, long arg1, long arg2) {
    switch (R_Tag) {
    case MiniboxConstants.INT:
      return MiniboxConversions.int2minibox(function2_apply_long_long_long_int(T1_Tag, T2_Tag, f, arg1, arg2));
    case MiniboxConstants.LONG:
      return MiniboxConversions.long2minibox(function2_apply_long_long_long_long(T1_Tag, T2_Tag, f, arg1, arg2));
    case MiniboxConstants.FLOAT:
      return MiniboxConversions.float2minibox(function2_apply_long_long_long_float(T1_Tag, T2_Tag, f, arg1, arg2));
    case MiniboxConstants.DOUBLE:
      return MiniboxConversions.double2minibox(function2_apply_long_long_long_double(T1_Tag, T2_Tag, f, arg1, arg2));
    default:
      return MiniboxConversions.box2minibox_tt(f.apply((T1)MiniboxConversions.minibox2box(arg1, T1_Tag), (T2)MiniboxConversions.minibox2box(arg2, T2_Tag)), R_Tag);
    }
  }

  @SuppressWarnings("unchecked")
  private static <T1, T2, R> int function2_apply_long_long_long_int(byte T1_Tag, byte T2_Tag, Function2<T1, T2, R> f, long arg1, long arg2) {
    switch (T1_Tag) {
    case MiniboxConstants.INT:
      return function2_apply_long_long_long_int_int(T2_Tag, f, MiniboxConversions.minibox2int(arg1), arg2);
    case MiniboxConstants.LONG:
      return function2_apply_long_long_long_int_long(T2_Tag, f, MiniboxConversions.minibox2long(arg1), arg2);
    case MiniboxConstants.DOUBLE:
      return function2_apply_long_long_long_int_double(T2_Tag, f, MiniboxConversions.minibox2double(arg1), arg2);
    default:
      return ((Integer)(f.apply((T1)MiniboxConversions.minibox2box(arg1, T1_Tag), (T2)MiniboxConversions.minibox2box(arg2, T2_Tag)))).intValue();
    }
  }

  @SuppressWarnings("unchecked")
  public static <T1, T2, R> int function2_apply_long_long_long_int_int(byte T2_Tag, Function2<T1, T2, R> f, int arg1, long arg2) {
    switch (T2_Tag) {
    case MiniboxConstants.INT:
      return f.apply$mcIII$sp(arg1, MiniboxConversions.minibox2int(arg2));
    case MiniboxConstants.LONG:
      return f.apply$mcIIJ$sp(arg1, MiniboxConversions.minibox2long(arg2));
    case MiniboxConstants.DOUBLE:
      return f.apply$mcIID$sp(arg1, MiniboxConversions.minibox2double(arg2));
    default:
      return ((Integer)(f.apply((T1)Integer.valueOf(arg1), (T2)MiniboxConversions.minibox2box(arg2, T2_Tag)))).intValue();
    }
  }

  @SuppressWarnings("unchecked")
  public static <T1, T2, R> int function2_apply_long_long_long_int_long(byte T2_Tag, Function2<T1, T2, R> f, long arg1, long arg2) {
    switch (T2_Tag) {
    case MiniboxConstants.INT:
      return f.apply$mcIJI$sp(arg1, MiniboxConversions.minibox2int(arg2));
    case MiniboxConstants.LONG:
      return f.apply$mcIJJ$sp(arg1, MiniboxConversions.minibox2long(arg2));
    case MiniboxConstants.DOUBLE:
      return f.apply$mcIJD$sp(arg1, MiniboxConversions.minibox2double(arg2));
    default:
      return ((Integer)(f.apply((T1)Long.valueOf(arg1), (T2)MiniboxConversions.minibox2box(arg2, T2_Tag)))).intValue();
    }
  }

  @SuppressWarnings("unchecked")
  public static <T1, T2, R> int function2_apply_long_long_long_int_double(byte T2_Tag, Function2<T1, T2, R> f, double arg1, long arg2) {
    switch (T2_Tag) {
    case MiniboxConstants.INT:
      return f.apply$mcIDI$sp(arg1, MiniboxConversions.minibox2int(arg2));
    case MiniboxConstants.LONG:
      return f.apply$mcIDJ$sp(arg1, MiniboxConversions.minibox2long(arg2));
    case MiniboxConstants.DOUBLE:
      return f.apply$mcIDD$sp(arg1, MiniboxConversions.minibox2double(arg2));
    default:
      return ((Integer)(f.apply((T1)Double.valueOf(arg1), (T2)MiniboxConversions.minibox2box(arg2, T2_Tag)))).intValue();
    }
  }

  @SuppressWarnings("unchecked")
  private static <T1, T2, R> long function2_apply_long_long_long_long(byte T1_Tag, byte T2_Tag, Function2<T1, T2, R> f, long arg1, long arg2) {
    switch (T1_Tag) {
    case MiniboxConstants.INT:
      return function2_apply_long_long_long_long_int(T2_Tag, f, MiniboxConversions.minibox2int(arg1), arg2);
    case MiniboxConstants.LONG:
      return function2_apply_long_long_long_long_long(T2_Tag, f, MiniboxConversions.minibox2long(arg1), arg2);
    case MiniboxConstants.DOUBLE:
      return function2_apply_long_long_long_long_double(T2_Tag, f, MiniboxConversions.minibox2double(arg1), arg2);
    default:
      return ((Long)(f.apply((T1)MiniboxConversions.minibox2box(arg1, T1_Tag), (T2)MiniboxConversions.minibox2box(arg2, T2_Tag)))).longValue();
    }
  }

  @SuppressWarnings("unchecked")
  public static <T1, T2, R> long function2_apply_long_long_long_long_int(byte T2_Tag, Function2<T1, T2, R> f, int arg1, long arg2) {
    switch (T2_Tag) {
    case MiniboxConstants.INT:
      return f.apply$mcJII$sp(arg1, MiniboxConversions.minibox2int(arg2));
    case MiniboxConstants.LONG:
      return f.apply$mcJIJ$sp(arg1, MiniboxConversions.minibox2long(arg2));
    case MiniboxConstants.DOUBLE:
      return f.apply$mcJID$sp(arg1, MiniboxConversions.minibox2double(arg2));
    default:
      return ((Long)(f.apply((T1)Integer.valueOf(arg1), (T2)MiniboxConversions.minibox2box(arg2, T2_Tag)))).longValue();
    }
  }

  @SuppressWarnings("unchecked")
  public static <T1, T2, R> long function2_apply_long_long_long_long_long(byte T2_Tag, Function2<T1, T2, R> f, long arg1, long arg2) {
    switch (T2_Tag) {
    case MiniboxConstants.INT:
      return f.apply$mcJJI$sp(arg1, MiniboxConversions.minibox2int(arg2));
    case MiniboxConstants.LONG:
      return f.apply$mcJJJ$sp(arg1, MiniboxConversions.minibox2long(arg2));
    case MiniboxConstants.DOUBLE:
      return f.apply$mcJJD$sp(arg1, MiniboxConversions.minibox2double(arg2));
    default:
      return ((Long)(f.apply((T1)Long.valueOf(arg1), (T2)MiniboxConversions.minibox2box(arg2, T2_Tag)))).longValue();
    }
  }

  @SuppressWarnings("unchecked")
  public static <T1, T2, R> long function2_apply_long_long_long_long_double(byte T2_Tag, Function2<T1, T2, R> f, double arg1, long arg2) {
    switch (T2_Tag) {
    case MiniboxConstants.INT:
      return f.apply$mcJDI$sp(arg1, MiniboxConversions.minibox2int(arg2));
    case MiniboxConstants.LONG:
      return f.apply$mcJDJ$sp(arg1, MiniboxConversions.minibox2long(arg2));
    case MiniboxConstants.DOUBLE:
      return f.apply$mcJDD$sp(arg1, MiniboxConversions.minibox2double(arg2));
    default:
      return ((Long)(f.apply((T1)Double.valueOf(arg1), (T2)MiniboxConversions.minibox2box(arg2, T2_Tag)))).longValue();
    }
  }

  @SuppressWarnings("unchecked")
  private static <T1, T2, R> float function2_apply_long_long_long_float(byte T1_Tag, byte T2_Tag, Function2<T1, T2, R> f, long arg1, long arg2) {
    switch (T1_Tag) {
    case MiniboxConstants.INT:
      return function2_apply_long_long_long_float_int(T2_Tag, f, MiniboxConversions.minibox2int(arg1), arg2);
    case MiniboxConstants.LONG:
      return function2_apply_long_long_long_float_long(T2_Tag, f, MiniboxConversions.minibox2long(arg1), arg2);
    case MiniboxConstants.DOUBLE:
      return function2_apply_long_long_long_float_double(T2_Tag, f, MiniboxConversions.minibox2double(arg1), arg2);
    default:
      return ((Float)(f.apply((T1)MiniboxConversions.minibox2box(arg1, T1_Tag), (T2)MiniboxConversions.minibox2box(arg2, T2_Tag)))).floatValue();
    }
  }

  @SuppressWarnings("unchecked")
  public static <T1, T2, R> float function2_apply_long_long_long_float_int(byte T2_Tag, Function2<T1, T2, R> f, int arg1, long arg2) {
    switch (T2_Tag) {
    case MiniboxConstants.INT:
      return f.apply$mcFII$sp(arg1, MiniboxConversions.minibox2int(arg2));
    case MiniboxConstants.LONG:
      return f.apply$mcFIJ$sp(arg1, MiniboxConversions.minibox2long(arg2));
    case MiniboxConstants.DOUBLE:
      return f.apply$mcFID$sp(arg1, MiniboxConversions.minibox2double(arg2));
    default:
      return ((Float)(f.apply((T1)Integer.valueOf(arg1), (T2)MiniboxConversions.minibox2box(arg2, T2_Tag)))).floatValue();
    }
  }

  @SuppressWarnings("unchecked")
  public static <T1, T2, R> float function2_apply_long_long_long_float_long(byte T2_Tag, Function2<T1, T2, R> f, long arg1, long arg2) {
    switch (T2_Tag) {
    case MiniboxConstants.INT:
      return f.apply$mcFJI$sp(arg1, MiniboxConversions.minibox2int(arg2));
    case MiniboxConstants.LONG:
      return f.apply$mcFJJ$sp(arg1, MiniboxConversions.minibox2long(arg2));
    case MiniboxConstants.DOUBLE:
      return f.apply$mcFJD$sp(arg1, MiniboxConversions.minibox2double(arg2));
    default:
      return ((Float)(f.apply((T1)Long.valueOf(arg1), (T2)MiniboxConversions.minibox2box(arg2, T2_Tag)))).floatValue();
    }
  }

  @SuppressWarnings("unchecked")
  public static <T1, T2, R> float function2_apply_long_long_long_float_double(byte T2_Tag, Function2<T1, T2, R> f, double arg1, long arg2) {
    switch (T2_Tag) {
    case MiniboxConstants.INT:
      return f.apply$mcFDI$sp(arg1, MiniboxConversions.minibox2int(arg2));
    case MiniboxConstants.LONG:
      return f.apply$mcFDJ$sp(arg1, MiniboxConversions.minibox2long(arg2));
    case MiniboxConstants.DOUBLE:
      return f.apply$mcFDD$sp(arg1, MiniboxConversions.minibox2double(arg2));
    default:
      return ((Float)(f.apply((T1)Double.valueOf(arg1), (T2)MiniboxConversions.minibox2box(arg2, T2_Tag)))).floatValue();
    }
  }

  @SuppressWarnings("unchecked")
  private static <T1, T2, R> double function2_apply_long_long_long_double(byte T1_Tag, byte T2_Tag, Function2<T1, T2, R> f, long arg1, long arg2) {
    switch (T1_Tag) {
    case MiniboxConstants.INT:
      return function2_apply_long_long_long_double_int(T2_Tag, f, MiniboxConversions.minibox2int(arg1), arg2);
    case MiniboxConstants.LONG:
      return function2_apply_long_long_long_double_long(T2_Tag, f, MiniboxConversions.minibox2long(arg1), arg2);
    case MiniboxConstants.DOUBLE:
      return function2_apply_long_long_long_double_double(T2_Tag, f, MiniboxConversions.minibox2double(arg1), arg2);
    default:
      return ((Double)(f.apply((T1)MiniboxConversions.minibox2box(arg1, T1_Tag), (T2)MiniboxConversions.minibox2box(arg2, T2_Tag)))).doubleValue();
    }
  }

  @SuppressWarnings("unchecked")
  public static <T1, T2, R> double function2_apply_long_long_long_double_int(byte T2_Tag, Function2<T1, T2, R> f, int arg1, long arg2) {
    switch (T2_Tag) {
    case MiniboxConstants.INT:
      return f.apply$mcDII$sp(arg1, MiniboxConversions.minibox2int(arg2));
    case MiniboxConstants.LONG:
      return f.apply$mcDIJ$sp(arg1, MiniboxConversions.minibox2long(arg2));
    case MiniboxConstants.DOUBLE:
      return f.apply$mcDID$sp(arg1, MiniboxConversions.minibox2double(arg2));
    default:
      return ((Double)(f.apply((T1)Integer.valueOf(arg1), (T2)MiniboxConversions.minibox2box(arg2, T2_Tag)))).doubleValue();
    }
  }

  @SuppressWarnings("unchecked")
  public static <T1, T2, R> double function2_apply_long_long_long_double_long(byte T2_Tag, Function2<T1, T2, R> f, long arg1, long arg2) {
    switch (T2_Tag) {
    case MiniboxConstants.INT:
      return f.apply$mcDJI$sp(arg1, MiniboxConversions.minibox2int(arg2));
    case MiniboxConstants.LONG:
      return f.apply$mcDJJ$sp(arg1, MiniboxConversions.minibox2long(arg2));
    case MiniboxConstants.DOUBLE:
      return f.apply$mcDJD$sp(arg1, MiniboxConversions.minibox2double(arg2));
    default:
      return ((Double)(f.apply((T1)Long.valueOf(arg1), (T2)MiniboxConversions.minibox2box(arg2, T2_Tag)))).doubleValue();
    }
  }

  @SuppressWarnings("unchecked")
  public static <T1, T2, R> double function2_apply_long_long_long_double_double(byte T2_Tag, Function2<T1, T2, R> f, double arg1, long arg2) {
    switch (T2_Tag) {
    case MiniboxConstants.INT:
      return f.apply$mcDDI$sp(arg1, MiniboxConversions.minibox2int(arg2));
    case MiniboxConstants.LONG:
      return f.apply$mcDDJ$sp(arg1, MiniboxConversions.minibox2long(arg2));
    case MiniboxConstants.DOUBLE:
      return f.apply$mcDDD$sp(arg1, MiniboxConversions.minibox2double(arg2));
    default:
      return ((Double)(f.apply((T1)Double.valueOf(arg1), (T2)MiniboxConversions.minibox2box(arg2, T2_Tag)))).doubleValue();
    }
  }
}
