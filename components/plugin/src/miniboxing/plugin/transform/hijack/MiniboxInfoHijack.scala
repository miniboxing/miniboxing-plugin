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
    if (sym.hasAnnotation(SpecializedClass) && flag_hijack_spec && currentRun.compiles(sym)) {
      sym.removeAnnotation(SpecializedClass)
      sym.resetFlag(SPECIALIZED)
      sym.addAnnotation(MinispecClass)
    }
    tpe
  }
}
