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

public class MiniboxFunctionBridge_v1 {

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
    switch (R_Tag + MULTIPLIER * T_Tag) {
    case MiniboxConstants.INT + MULTIPLIER * MiniboxConstants.INT:
      return MiniboxConversions.int2minibox(f.apply$mcII$sp(MiniboxConversions.minibox2int(arg)));
    case MiniboxConstants.INT + MULTIPLIER * MiniboxConstants.LONG:
      return MiniboxConversions.int2minibox(f.apply$mcIJ$sp(MiniboxConversions.minibox2long(arg)));
    case MiniboxConstants.INT + MULTIPLIER * MiniboxConstants.FLOAT:
      return MiniboxConversions.int2minibox(f.apply$mcIF$sp(MiniboxConversions.minibox2float(arg)));
    case MiniboxConstants.INT + MULTIPLIER * MiniboxConstants.DOUBLE:
      return MiniboxConversions.int2minibox(f.apply$mcID$sp(MiniboxConversions.minibox2double(arg)));
    case MiniboxConstants.LONG + MULTIPLIER * MiniboxConstants.INT:
      return MiniboxConversions.long2minibox(f.apply$mcJI$sp(MiniboxConversions.minibox2int(arg)));
    case MiniboxConstants.LONG + MULTIPLIER * MiniboxConstants.LONG:
      return MiniboxConversions.long2minibox(f.apply$mcJJ$sp(MiniboxConversions.minibox2long(arg)));
    case MiniboxConstants.LONG + MULTIPLIER * MiniboxConstants.FLOAT:
      return MiniboxConversions.long2minibox(f.apply$mcJF$sp(MiniboxConversions.minibox2float(arg)));
    case MiniboxConstants.LONG + MULTIPLIER * MiniboxConstants.DOUBLE:
      return MiniboxConversions.long2minibox(f.apply$mcJD$sp(MiniboxConversions.minibox2double(arg)));
    case MiniboxConstants.FLOAT + MULTIPLIER * MiniboxConstants.INT:
      return MiniboxConversions.float2minibox(f.apply$mcFI$sp(MiniboxConversions.minibox2int(arg)));
    case MiniboxConstants.FLOAT + MULTIPLIER * MiniboxConstants.LONG:
      return MiniboxConversions.float2minibox(f.apply$mcFJ$sp(MiniboxConversions.minibox2long(arg)));
    case MiniboxConstants.FLOAT + MULTIPLIER * MiniboxConstants.FLOAT:
      return MiniboxConversions.float2minibox(f.apply$mcFF$sp(MiniboxConversions.minibox2float(arg)));
    case MiniboxConstants.FLOAT + MULTIPLIER * MiniboxConstants.DOUBLE:
      return MiniboxConversions.float2minibox(f.apply$mcFD$sp(MiniboxConversions.minibox2double(arg)));
    case MiniboxConstants.DOUBLE + MULTIPLIER * MiniboxConstants.INT:
      return MiniboxConversions.double2minibox(f.apply$mcDI$sp(MiniboxConversions.minibox2int(arg)));
    case MiniboxConstants.DOUBLE + MULTIPLIER * MiniboxConstants.LONG:
      return MiniboxConversions.double2minibox(f.apply$mcDJ$sp(MiniboxConversions.minibox2long(arg)));
    case MiniboxConstants.DOUBLE + MULTIPLIER * MiniboxConstants.FLOAT:
      return MiniboxConversions.double2minibox(f.apply$mcDF$sp(MiniboxConversions.minibox2float(arg)));
    case MiniboxConstants.DOUBLE + MULTIPLIER * MiniboxConstants.DOUBLE:
      return MiniboxConversions.double2minibox(f.apply$mcDD$sp(MiniboxConversions.minibox2double(arg)));
    default:
      return MiniboxConversions.box2minibox_tt(f.apply((T)MiniboxConversions.minibox2box(arg, T_Tag)), R_Tag);
    }
  }

  @SuppressWarnings("unchecked")
  public static <T1, T2, R> long function2_apply_long_long_long(byte T1_Tag, byte T2_Tag, byte R_Tag, Function2<T1, T2, R> f, long arg1, long arg2) {
    switch (R_Tag + MULTIPLIER * T1_Tag + MULTIPLIER * MULTIPLIER * T2_Tag) {
    case MiniboxConstants.INT + MULTIPLIER * MiniboxConstants.INT + MULTIPLIER * MULTIPLIER * MiniboxConstants.INT:
      return MiniboxConversions.int2minibox(f.apply$mcIII$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2int(arg2)));
    case MiniboxConstants.INT + MULTIPLIER * MiniboxConstants.LONG + MULTIPLIER * MULTIPLIER * MiniboxConstants.INT:
      return MiniboxConversions.int2minibox(f.apply$mcIJI$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2int(arg2)));
    case MiniboxConstants.INT + MULTIPLIER * MiniboxConstants.DOUBLE + MULTIPLIER * MULTIPLIER * MiniboxConstants.INT:
      return MiniboxConversions.int2minibox(f.apply$mcIDI$sp(MiniboxConversions.minibox2double(arg1), MiniboxConversions.minibox2int(arg2)));
    case MiniboxConstants.INT + MULTIPLIER * MiniboxConstants.INT + MULTIPLIER * MULTIPLIER * MiniboxConstants.LONG:
      return MiniboxConversions.int2minibox(f.apply$mcIIJ$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2long(arg2)));
    case MiniboxConstants.INT + MULTIPLIER * MiniboxConstants.LONG + MULTIPLIER * MULTIPLIER * MiniboxConstants.LONG:
      return MiniboxConversions.int2minibox(f.apply$mcIJJ$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2long(arg2)));
    case MiniboxConstants.INT + MULTIPLIER * MiniboxConstants.DOUBLE + MULTIPLIER * MULTIPLIER * MiniboxConstants.LONG:
      return MiniboxConversions.int2minibox(f.apply$mcIDJ$sp(MiniboxConversions.minibox2double(arg1), MiniboxConversions.minibox2long(arg2)));
    case MiniboxConstants.INT + MULTIPLIER * MiniboxConstants.INT + MULTIPLIER * MULTIPLIER * MiniboxConstants.DOUBLE:
      return MiniboxConversions.int2minibox(f.apply$mcIID$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2double(arg2)));
    case MiniboxConstants.INT + MULTIPLIER * MiniboxConstants.LONG + MULTIPLIER * MULTIPLIER * MiniboxConstants.DOUBLE:
      return MiniboxConversions.int2minibox(f.apply$mcIJD$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2double(arg2)));
    case MiniboxConstants.INT + MULTIPLIER * MiniboxConstants.DOUBLE + MULTIPLIER * MULTIPLIER * MiniboxConstants.DOUBLE:
      return MiniboxConversions.int2minibox(f.apply$mcIDD$sp(MiniboxConversions.minibox2double(arg1), MiniboxConversions.minibox2double(arg2)));
    case MiniboxConstants.LONG + MULTIPLIER * MiniboxConstants.INT + MULTIPLIER * MULTIPLIER * MiniboxConstants.INT:
      return MiniboxConversions.long2minibox(f.apply$mcJII$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2int(arg2)));
    case MiniboxConstants.LONG + MULTIPLIER * MiniboxConstants.LONG + MULTIPLIER * MULTIPLIER * MiniboxConstants.INT:
      return MiniboxConversions.long2minibox(f.apply$mcJJI$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2int(arg2)));
    case MiniboxConstants.LONG + MULTIPLIER * MiniboxConstants.DOUBLE + MULTIPLIER * MULTIPLIER * MiniboxConstants.INT:
      return MiniboxConversions.long2minibox(f.apply$mcJDI$sp(MiniboxConversions.minibox2double(arg1), MiniboxConversions.minibox2int(arg2)));
    case MiniboxConstants.LONG + MULTIPLIER * MiniboxConstants.INT + MULTIPLIER * MULTIPLIER * MiniboxConstants.LONG:
      return MiniboxConversions.long2minibox(f.apply$mcJIJ$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2long(arg2)));
    case MiniboxConstants.LONG + MULTIPLIER * MiniboxConstants.LONG + MULTIPLIER * MULTIPLIER * MiniboxConstants.LONG:
      return MiniboxConversions.long2minibox(f.apply$mcJJJ$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2long(arg2)));
    case MiniboxConstants.LONG + MULTIPLIER * MiniboxConstants.DOUBLE + MULTIPLIER * MULTIPLIER * MiniboxConstants.LONG:
      return MiniboxConversions.long2minibox(f.apply$mcJDJ$sp(MiniboxConversions.minibox2double(arg1), MiniboxConversions.minibox2long(arg2)));
    case MiniboxConstants.LONG + MULTIPLIER * MiniboxConstants.INT + MULTIPLIER * MULTIPLIER * MiniboxConstants.DOUBLE:
      return MiniboxConversions.long2minibox(f.apply$mcJID$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2double(arg2)));
    case MiniboxConstants.LONG + MULTIPLIER * MiniboxConstants.LONG + MULTIPLIER * MULTIPLIER * MiniboxConstants.DOUBLE:
      return MiniboxConversions.long2minibox(f.apply$mcJJD$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2double(arg2)));
    case MiniboxConstants.LONG + MULTIPLIER * MiniboxConstants.DOUBLE + MULTIPLIER * MULTIPLIER * MiniboxConstants.DOUBLE:
      return MiniboxConversions.long2minibox(f.apply$mcJDD$sp(MiniboxConversions.minibox2double(arg1), MiniboxConversions.minibox2double(arg2)));
    case MiniboxConstants.FLOAT + MULTIPLIER * MiniboxConstants.INT + MULTIPLIER * MULTIPLIER * MiniboxConstants.INT:
      return MiniboxConversions.float2minibox(f.apply$mcFII$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2int(arg2)));
    case MiniboxConstants.FLOAT + MULTIPLIER * MiniboxConstants.LONG + MULTIPLIER * MULTIPLIER * MiniboxConstants.INT:
      return MiniboxConversions.float2minibox(f.apply$mcFJI$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2int(arg2)));
    case MiniboxConstants.FLOAT + MULTIPLIER * MiniboxConstants.DOUBLE + MULTIPLIER * MULTIPLIER * MiniboxConstants.INT:
      return MiniboxConversions.float2minibox(f.apply$mcFDI$sp(MiniboxConversions.minibox2double(arg1), MiniboxConversions.minibox2int(arg2)));
    case MiniboxConstants.FLOAT + MULTIPLIER * MiniboxConstants.INT + MULTIPLIER * MULTIPLIER * MiniboxConstants.LONG:
      return MiniboxConversions.float2minibox(f.apply$mcFIJ$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2long(arg2)));
    case MiniboxConstants.FLOAT + MULTIPLIER * MiniboxConstants.LONG + MULTIPLIER * MULTIPLIER * MiniboxConstants.LONG:
      return MiniboxConversions.float2minibox(f.apply$mcFJJ$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2long(arg2)));
    case MiniboxConstants.FLOAT + MULTIPLIER * MiniboxConstants.DOUBLE + MULTIPLIER * MULTIPLIER * MiniboxConstants.LONG:
      return MiniboxConversions.float2minibox(f.apply$mcFDJ$sp(MiniboxConversions.minibox2double(arg1), MiniboxConversions.minibox2long(arg2)));
    case MiniboxConstants.FLOAT + MULTIPLIER * MiniboxConstants.INT + MULTIPLIER * MULTIPLIER * MiniboxConstants.DOUBLE:
      return MiniboxConversions.float2minibox(f.apply$mcFID$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2double(arg2)));
    case MiniboxConstants.FLOAT + MULTIPLIER * MiniboxConstants.LONG + MULTIPLIER * MULTIPLIER * MiniboxConstants.DOUBLE:
      return MiniboxConversions.float2minibox(f.apply$mcFJD$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2double(arg2)));
    case MiniboxConstants.FLOAT + MULTIPLIER * MiniboxConstants.DOUBLE + MULTIPLIER * MULTIPLIER * MiniboxConstants.DOUBLE:
      return MiniboxConversions.float2minibox(f.apply$mcFDD$sp(MiniboxConversions.minibox2double(arg1), MiniboxConversions.minibox2double(arg2)));
    case MiniboxConstants.DOUBLE + MULTIPLIER * MiniboxConstants.INT + MULTIPLIER * MULTIPLIER * MiniboxConstants.INT:
      return MiniboxConversions.double2minibox(f.apply$mcDII$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2int(arg2)));
    case MiniboxConstants.DOUBLE + MULTIPLIER * MiniboxConstants.LONG + MULTIPLIER * MULTIPLIER * MiniboxConstants.INT:
      return MiniboxConversions.double2minibox(f.apply$mcDJI$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2int(arg2)));
    case MiniboxConstants.DOUBLE + MULTIPLIER * MiniboxConstants.DOUBLE + MULTIPLIER * MULTIPLIER * MiniboxConstants.INT:
      return MiniboxConversions.double2minibox(f.apply$mcDDI$sp(MiniboxConversions.minibox2double(arg1), MiniboxConversions.minibox2int(arg2)));
    case MiniboxConstants.DOUBLE + MULTIPLIER * MiniboxConstants.INT + MULTIPLIER * MULTIPLIER * MiniboxConstants.LONG:
      return MiniboxConversions.double2minibox(f.apply$mcDIJ$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2long(arg2)));
    case MiniboxConstants.DOUBLE + MULTIPLIER * MiniboxConstants.LONG + MULTIPLIER * MULTIPLIER * MiniboxConstants.LONG:
      return MiniboxConversions.double2minibox(f.apply$mcDJJ$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2long(arg2)));
    case MiniboxConstants.DOUBLE + MULTIPLIER * MiniboxConstants.DOUBLE + MULTIPLIER * MULTIPLIER * MiniboxConstants.LONG:
      return MiniboxConversions.double2minibox(f.apply$mcDDJ$sp(MiniboxConversions.minibox2double(arg1), MiniboxConversions.minibox2long(arg2)));
    case MiniboxConstants.DOUBLE + MULTIPLIER * MiniboxConstants.INT + MULTIPLIER * MULTIPLIER * MiniboxConstants.DOUBLE:
      return MiniboxConversions.double2minibox(f.apply$mcDID$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2double(arg2)));
    case MiniboxConstants.DOUBLE + MULTIPLIER * MiniboxConstants.LONG + MULTIPLIER * MULTIPLIER * MiniboxConstants.DOUBLE:
      return MiniboxConversions.double2minibox(f.apply$mcDJD$sp(MiniboxConversions.minibox2int(arg1), MiniboxConversions.minibox2double(arg2)));
    case MiniboxConstants.DOUBLE + MULTIPLIER * MiniboxConstants.DOUBLE + MULTIPLIER * MULTIPLIER * MiniboxConstants.DOUBLE:
      return MiniboxConversions.double2minibox(f.apply$mcDDD$sp(MiniboxConversions.minibox2double(arg1), MiniboxConversions.minibox2double(arg2)));
    default:
      return MiniboxConversions.box2minibox_tt(f.apply((T1)MiniboxConversions.minibox2box(arg1, T1_Tag), (T2)MiniboxConversions.minibox2box(arg2, T2_Tag)), R_Tag);
    }
  }
}
