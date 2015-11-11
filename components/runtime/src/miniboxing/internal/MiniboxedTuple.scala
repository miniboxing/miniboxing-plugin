//
//     _____   .__         .__ ___.                    .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_ |__    ____  ___  ___|__|  ____     ____
//   /  \ /  \ |  | /    \ |  | | __ \  /  _ \ \  \/  /|  | /    \   / ___\
//  /    Y    \|  ||   |  \|  | | \_\ \(  <_> ) >    < |  ||   |  \ / /_/  >
//  \____|__  /|__||___|  /|__| |___  / \____/ /__/\_ \|__||___|  / \___  /
//          \/          \/          \/               \/         \/ /_____/
// Copyright (c) 2011-2015 Scala Team, École polytechnique fédérale de Lausanne
//
// Authors:
//    * Milos Stojanovic
//

package miniboxing.internal

object MiniboxedTuple {

  // Accessors

  // Tuple1

  def tuple1_accessor_1_long[T1](T1_Tag: Byte, _t: Tuple1[T1]): Long =
    (T1_Tag) match {
      case MiniboxConstants.INT =>
        MiniboxConversions.int2minibox(_t.asInstanceOf[Tuple1[Int]]._1)
      case MiniboxConstants.LONG =>
        MiniboxConversions.long2minibox(_t.asInstanceOf[Tuple1[Long]]._1)
      case _ =>
        MiniboxConversions.box2minibox_tt(_t._1, T1_Tag)
    }

  def tuple1_accessor_1_double[T1](T1_Tag: Byte, _t: Tuple1[T1]): Double =
    (T1_Tag) match {
      case MiniboxConstants.DOUBLE =>
        MiniboxConversions.double2minibox(_t.asInstanceOf[Tuple1[Double]]._1)
      case _ =>
        MiniboxConversions.box2minibox_tt(_t._1, T1_Tag)
    }

  // Tuple2

  def tuple2_accessor_1_long[T1, T2](T1_Tag: Byte, _t: Tuple2[T1, T2]): Long =
    (T1_Tag) match {
      case MiniboxConstants.INT =>
        MiniboxConversions.int2minibox(_t.asInstanceOf[Tuple2[Int, Int]]._1)
      case MiniboxConstants.LONG =>
        MiniboxConversions.long2minibox(_t.asInstanceOf[Tuple2[Long, Int]]._1)
      case MiniboxConstants.BOOLEAN =>
        MiniboxConversions.boolean2minibox(_t.asInstanceOf[Tuple2[Boolean, Int]]._1)
      case MiniboxConstants.CHAR =>
        MiniboxConversions.char2minibox(_t.asInstanceOf[Tuple2[Char, Int]]._1)
      case _ =>
        MiniboxConversions.box2minibox_tt(_t._1, T1_Tag)
    }

  def tuple2_accessor_1_double[T1, T2](T1_Tag: Byte, _t: Tuple2[T1, T2]): Double =
    (T1_Tag) match {
      case MiniboxConstants.DOUBLE =>
        MiniboxConversions.double2minibox(_t.asInstanceOf[Tuple2[Double, T2]]._1)
      case _ =>
        MiniboxConversions.box2minibox_tt(_t._1, T1_Tag)
    }

  def tuple2_accessor_2_long[T1, T2](T2_Tag: Byte, _t: Tuple2[T1, T2]): Long =
    (T2_Tag) match {
      case MiniboxConstants.INT =>
        MiniboxConversions.int2minibox(_t.asInstanceOf[Tuple2[Int, Int]]._2)
      case MiniboxConstants.LONG =>
        MiniboxConversions.long2minibox(_t.asInstanceOf[Tuple2[Int, Long]]._2)
      case MiniboxConstants.BOOLEAN =>
        MiniboxConversions.boolean2minibox(_t.asInstanceOf[Tuple2[Int, Boolean]]._2)
      case MiniboxConstants.CHAR =>
        MiniboxConversions.char2minibox(_t.asInstanceOf[Tuple2[Int, Char]]._2)
      case _ =>
        MiniboxConversions.box2minibox_tt(_t._2, T2_Tag)
    }

  def tuple2_accessor_2_double[T1, T2](T2_Tag: Byte, _t: Tuple2[T1, T2]): Double =
    (T2_Tag) match {
      case MiniboxConstants.DOUBLE =>
        MiniboxConversions.double2minibox(_t.asInstanceOf[Tuple2[Int, Double]]._2)
      case _ =>
        MiniboxConversions.box2minibox_tt(_t._2, T2_Tag)
    }

  // Constructors

  // Tuple1

  def newTuple1_long[T1](T1_Tag: Byte, t1: Long): Tuple1[T1] =
    ((T1_Tag) match {
      case MiniboxConstants.INT =>
        new Tuple1[Int](MiniboxConversions.minibox2int(t1))
      case MiniboxConstants.LONG =>
        new Tuple1[Long](MiniboxConversions.minibox2long(t1))
      case MiniboxConstants.CHAR =>
        new Tuple1[Char](MiniboxConversions.minibox2char(t1))
      case MiniboxConstants.BOOLEAN =>
        new Tuple1[Boolean](MiniboxConversions.minibox2boolean(t1))
      case _ =>
        new Tuple1[T1](MiniboxConversionsLong.minibox2box[T1](t1, T1_Tag))
    }).asInstanceOf[Tuple1[T1]]

  def newTuple1_double[T1](T1_Tag: Byte, t1: Double): Tuple1[T1] =
    ((T1_Tag) match {
      case MiniboxConstants.DOUBLE =>
        new Tuple1[Double](MiniboxConversionsDouble.minibox2double(t1))
      case _ =>
        new Tuple1[T1](MiniboxConversionsDouble.minibox2box[T1](t1, T1_Tag))
    }).asInstanceOf[Tuple1[T1]]

  // Tuple2

  def newTuple2_long_long[T1, T2](T1_Tag: Byte, T2_Tag: Byte, t1: Long, t2: Long): Tuple2[T1, T2] =
    ((T1_Tag * 10 + T2_Tag) match {
      case 55 /* INT * 10 + INT */ =>
        new Tuple2[Int, Int](MiniboxConversions.minibox2int(t1), MiniboxConversions.minibox2int(t2))
      case 56 /* INT * 10 + LONG */ =>
        new Tuple2[Int, Long](MiniboxConversions.minibox2int(t1), MiniboxConversions.minibox2long(t2))
      case 54 /* INT * 10 + CHAR */ =>
        new Tuple2[Int, Char](MiniboxConversions.minibox2int(t1), MiniboxConversions.minibox2char(t2))
      case 51 /* INT * 10 + BOOLEAN */ =>
        new Tuple2[Int, Boolean](MiniboxConversions.minibox2int(t1), MiniboxConversions.minibox2boolean(t2))
      case 65 /* LONG * 10 + INT */ =>
        new Tuple2[Long, Int](MiniboxConversions.minibox2long(t1), MiniboxConversions.minibox2int(t2))
      case 66 /* LONG * 10 + LONG */ =>
        new Tuple2[Long, Long](MiniboxConversions.minibox2long(t1), MiniboxConversions.minibox2long(t2))
      case 64 /* LONG * 10 + CHAR */ =>
        new Tuple2[Long, Char](MiniboxConversions.minibox2long(t1), MiniboxConversions.minibox2char(t2))
      case 61 /* LONG * 10 + BOOLEAN */ =>
        new Tuple2[Long, Boolean](MiniboxConversions.minibox2long(t1), MiniboxConversions.minibox2boolean(t2))
      case 45 /* CHAR * 10 + INT */ =>
        new Tuple2[Char, Int](MiniboxConversions.minibox2char(t1), MiniboxConversions.minibox2int(t2))
      case 46 /* CHAR * 10 + LONG */ =>
        new Tuple2[Char, Long](MiniboxConversions.minibox2char(t1), MiniboxConversions.minibox2long(t2))
      case 44 /* CHAR * 10 + CHAR */ =>
        new Tuple2[Char, Char](MiniboxConversions.minibox2char(t1), MiniboxConversions.minibox2char(t2))
      case 41 /* CHAR * 10 + BOOLEAN */ =>
        new Tuple2[Char, Boolean](MiniboxConversions.minibox2char(t1), MiniboxConversions.minibox2boolean(t2))
      case 15 /* BOOLEAN * 10 + INT */ =>
        new Tuple2[Boolean, Int](MiniboxConversions.minibox2boolean(t1), MiniboxConversions.minibox2int(t2))
      case 16 /* BOOLEAN * 10 + LONG */ =>
        new Tuple2[Boolean, Long](MiniboxConversions.minibox2boolean(t1), MiniboxConversions.minibox2long(t2))
      case 14 /* BOOLEAN * 10 + CHAR */ =>
        new Tuple2[Boolean, Char](MiniboxConversions.minibox2boolean(t1), MiniboxConversions.minibox2char(t2))
      case 11 /* BOOLEAN * 10 + BOOLEAN */ =>
        new Tuple2[Boolean, Boolean](MiniboxConversions.minibox2boolean(t1), MiniboxConversions.minibox2boolean(t2))
      case _ =>
        new Tuple2[T1, T2](MiniboxConversionsLong.minibox2box[T1](t1, T1_Tag), MiniboxConversionsLong.minibox2box[T2](t2, T2_Tag))
    }).asInstanceOf[Tuple2[T1, T2]]

  def newTuple2_long_double[T1, T2](T1_Tag: Byte, T2_Tag: Byte, t1: Long, t2: Double): Tuple2[T1, T2] =
    ((T1_Tag * 10 + T2_Tag) match {
      case 58 /* INT * 10 + DOUBLE */ =>
        new Tuple2[Int, Double](MiniboxConversions.minibox2int(t1), MiniboxConversionsDouble.minibox2double(t2))
      case 68 /* LONG * 10 + DOUBLE */ =>
        new Tuple2[Long, Double](MiniboxConversions.minibox2long(t1), MiniboxConversionsDouble.minibox2double(t2))
      case 48 /* CHAR * 10 + DOUBLE */ =>
        new Tuple2[Char, Double](MiniboxConversions.minibox2char(t1), MiniboxConversionsDouble.minibox2double(t2))
      case 18 /* BOOLEAN * 10 + DOUBLE */ =>
        new Tuple2[Boolean, Double](MiniboxConversions.minibox2boolean(t1), MiniboxConversionsDouble.minibox2double(t2))
      case _ =>
        new Tuple2[T1, T2](MiniboxConversionsLong.minibox2box[T1](t1, T1_Tag), MiniboxConversionsDouble.minibox2box[T2](t2, T2_Tag))
    }).asInstanceOf[Tuple2[T1, T2]]

  def newTuple2_double_long[T1, T2](T1_Tag: Byte, T2_Tag: Byte, t1: Double, t2: Long): Tuple2[T1, T2] =
    ((T1_Tag * 10 + T2_Tag) match {
      case 85 /* DOUBLE * 10 + INT */ =>
        new Tuple2[Double, Int](MiniboxConversionsDouble.minibox2double(t1), MiniboxConversions.minibox2int(t2))
      case 86 /* DOUBLE * 10 + LONG */ =>
        new Tuple2[Double, Long](MiniboxConversionsDouble.minibox2double(t1), MiniboxConversions.minibox2long(t2))
      case 84 /* DOUBLE * 10 + LONG */ =>
        new Tuple2[Double, Char](MiniboxConversionsDouble.minibox2double(t1), MiniboxConversions.minibox2char(t2))
      case 81 /* DOUBLE * 10 + BOOLEAN */ =>
        new Tuple2[Double, Boolean](MiniboxConversionsDouble.minibox2double(t1), MiniboxConversions.minibox2boolean(t2))
      case _ =>
        new Tuple2[T1, T2](MiniboxConversionsDouble.minibox2box[T1](t1, T1_Tag), MiniboxConversionsLong.minibox2box[T2](t2, T2_Tag))
    }).asInstanceOf[Tuple2[T1, T2]]

  def newTuple2_double_double[T1, T2](T1_Tag: Byte, T2_Tag: Byte, t1: Double, t2: Double): Tuple2[T1, T2] =
    ((T1_Tag * 10 + T2_Tag) match {
      case 88 /* DOUBLE * 10 + DOUBLE */ =>
        new Tuple2[Double, Double](MiniboxConversionsDouble.minibox2double(t1), MiniboxConversionsDouble.minibox2double(t2))
      case _ =>
        new Tuple2[T1, T2](MiniboxConversionsDouble.minibox2box[T1](t1, T1_Tag), MiniboxConversionsDouble.minibox2box[T2](t2, T2_Tag))
    }).asInstanceOf[Tuple2[T1, T2]]
}
