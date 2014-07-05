package miniboxing
package plugin
package transform
package interop
package inject

import scala.tools.nsc.transform.InfoTransform

trait InteropInjectInfoTransformer extends InfoTransform {
  self: InteropInjectComponent =>

  import global._
  import definitions._

  override def transformInfo(sym: Symbol, tpe: Type): Type =
    tpe
}