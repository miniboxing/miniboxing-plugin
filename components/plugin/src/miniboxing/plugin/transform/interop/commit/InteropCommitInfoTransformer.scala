package miniboxing
package plugin
package transform
package interop
package commit

import scala.tools.nsc.transform.InfoTransform

trait InteropCommitInfoTransformer extends InfoTransform {
  self: InteropCommitComponent =>

  import global._
  import definitions._

  override def transformInfo(sym: Symbol, tpe: Type): Type =
    tpe
}