package miniboxing.plugin

import scala.tools.nsc.transform.InfoTransform

trait MiniboxPostInfoTransformer extends InfoTransform {
  this: MiniboxSpecComponent =>

  import global._
  import definitions._
  import minibox._

  override def transformInfo(sym: Symbol, tpe: Type): Type = {
    val tpe2 = deepTransformation(tpe)
//    if (!(tpe =:= tpe2))
//      println(sym + "  old: " + tpe + "  new: " + tpe2)
    tpe2
  }

  lazy val deepTransformation: TypeMap = new TypeMap {
    def apply(tpe: Type): Type = mapOver(tpe)
    override def mapOver(tpe: Type): Type = tpe match {
      case tpe if tpe.hasAnnotation(StorageClass) =>
        LongTpe
      case _ =>
        super.mapOver(tpe)
    }
  }
}
