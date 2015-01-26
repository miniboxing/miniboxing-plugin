//
//     _____   .__         .__ ____.                     .__ scala-miniboxing.org
//    /_   _\  |__|  ____  |__|\_  |__    _____  ___  ___|__|  ____    _____
//   / o\ /o \ |  | /    \ |  | |  __ \  /  ___\ \  \/  /|  | /    \  /  ___\
//  /    Y    \|  ||   |  \|  | |  \_\ \(  (_)  ) >    < |  ||   |  \(  /_/  )
//  \____|__  /|__||___|  /|__| |____  / \_____/ /__/\_ \|__||___|  / \___  /
//          \/          \/           \/                \/         \/ /_____/
// Copyright (c) 2011-2015 Scala Team, École polytechnique fédérale de Lausanne
//
// Authors:
//    * Vlad Ureche
//    * Nicolas Stucki
//
// Thanks to: @tixxit (Tom Switzer), @dlwh (David Hall) and @ichoran (Rex Kerr)
// for their very good feedback!
//
package miniboxing.runtime
package reflect

import scala.reflect.ClassTag
import miniboxing.runtime.MiniboxConstants._

object MbReflectionImpl {

  import MbReflection.SimpleType

  def storageTypeImpl(T_Tag: Byte) = {
    val LONG_REPR = SimpleType.long
    val DOUB_REPR = SimpleType.double
    val REFR_REPR = SimpleType.reference
    T_Tag match {
      case UNIT    => LONG_REPR
      case BOOLEAN => LONG_REPR
      case BYTE    => LONG_REPR
      case CHAR    => LONG_REPR
      case SHORT   => LONG_REPR
      case INT     => LONG_REPR
      case LONG    => LONG_REPR
      case FLOAT   => DOUB_REPR
      case DOUBLE  => DOUB_REPR
      case _       => REFR_REPR
    }
  }

  def reifiedTypeImpl(T_Tag: Byte) = {
    T_Tag match {
      case UNIT    => SimpleType.unit
      case BOOLEAN => SimpleType.boolean
      case BYTE    => SimpleType.byte
      case CHAR    => SimpleType.char
      case SHORT   => SimpleType.short
      case INT     => SimpleType.int
      case LONG    => SimpleType.long
      case FLOAT   => SimpleType.float
      case DOUBLE  => SimpleType.double
      case _       => SimpleType.reference
    }
  }
}