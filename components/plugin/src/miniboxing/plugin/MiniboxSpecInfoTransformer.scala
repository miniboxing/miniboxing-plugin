package miniboxing.plugin

import scala.tools.nsc.transform.InfoTransform

trait MiniboxPostInfoTransformer extends InfoTransform {
  this: MiniboxSpecComponent =>

  import global._

  override def transformInfo(sym: Symbol, tpe: Type): Type = tpe
}
