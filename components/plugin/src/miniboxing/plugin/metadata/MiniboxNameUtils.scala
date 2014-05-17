package miniboxing.plugin
package metadata

import scala.Option.option2Iterable
import scala.collection.immutable

trait MiniboxNameUtils {
  self: MiniboxDuplComponent =>

  import global._
  import definitions._
  import scala.collection.immutable

  /**
   * Specialize name for the two list of types.
   */
  def specializedName(name: Name, types: List[Type]): TermName = {
    if (nme.CONSTRUCTOR == name || (types.isEmpty))
      name
    else if (nme.isSetterName(name))
      nme.getterToSetter(specializedName(nme.setterToGetter(name), types))
    else if (nme.isLocalName(name))
      nme.getterToLocal(specializedName(nme.localToGetter(name), types))
    else {
      newTermName(name.toString + "_" + types.map(t => definitions.abbrvTag(t.typeSymbol)).mkString(""))
    }
  }

  /**
   * The name of the field carrying the type tag of corresponding to a type
   * parameter `tparam`
   */
  def typeTagName(clazz: Symbol, tparam: Symbol): TermName =
    // See #55 for an explanation of why I did this: https://github.com/miniboxing/miniboxing-plugin/issues/55
    newTermName(clazz.fullName('|') + "|" + shortTypeTagName(tparam))

  def shortTypeTagName(tparam: Symbol): TermName =
    newTermName(tparam.name.toString + "_TypeTag")

  def isTypeTagField(field: Symbol): Boolean = {
    field.name.endsWith("_TypeTag")
  }

}
