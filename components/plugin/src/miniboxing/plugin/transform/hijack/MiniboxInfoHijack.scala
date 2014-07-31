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
package miniboxing.plugin
package transform
package hijack

import scala.tools.nsc.transform.InfoTransform
import miniboxing.plugin.HijackComponent
import scala.reflect.internal.Flags.SPECIALIZED

trait MiniboxInfoHijack extends InfoTransform {
  this: HijackComponent =>

  import global._
  import definitions._
  import scala.reflect.internal.Flags._

  def transformInfo(sym: Symbol, tpe: Type): Type = {
    if (sym.isTypeParameter && currentRun.compiles(sym) && ((sym.hasAnnotation(SpecializedClass) && flag_hijack_spec) || flag_mark_all)) {
      sym.removeAnnotation(SpecializedClass)
      sym.removeAnnotation(MinispecClass)
      sym.resetFlag(SPECIALIZED)
      sym.addAnnotation(MinispecClass)
    }
    tpe
  }
}
