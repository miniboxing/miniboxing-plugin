package miniboxing.plugin

import scala.tools.nsc.transform.InfoTransform

trait MiniboxPostInfoTransformer extends InfoTransform {
  this: MiniboxSpecComponent =>

  import global._
  import definitions._
  import minibox._

  override def transformInfo(sym: Symbol, tpe: Type): Type = {
    val tpe2 = tpe match {
      case MethodType(args, ret) =>
        MethodType(args, transform(ret))
      case _ =>
        transform(tpe)
    }
//    if (!(tpe =:= tpe2))
//      println(sym + "  old: " + tpe + "  new: " + tpe2)
    tpe2
  }

  def transform(tpe: Type): Type =
    if (tpe.hasAnnotation(StorageClass))
      LongTpe
    else
      tpe
}
