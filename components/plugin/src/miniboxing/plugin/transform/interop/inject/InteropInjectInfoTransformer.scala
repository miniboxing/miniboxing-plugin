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
    if (currentRun.compiles(sym)) {
      if (tpe.typeSymbol == Function0Class)
        tpe.withMbFunction
      else if (tpe.typeSymbol == Function1Class)
        tpe.withMbFunction
      else if (tpe.typeSymbol == Function2Class)
        tpe.withMbFunction
      else
        tpe
    } else
      tpe
}