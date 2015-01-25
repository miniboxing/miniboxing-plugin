//
//     _____   .__         .__ ___.                    .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_ |__    ____  ___  ___|__|  ____     ____
//   /  \ /  \ |  | /    \ |  | | __ \  /  _ \ \  \/  /|  | /    \   / ___\
//  /    Y    \|  ||   |  \|  | | \_\ \(  <_> ) >    < |  ||   |  \ / /_/  >
//  \____|__  /|__||___|  /|__| |___  / \____/ /__/\_ \|__||___|  / \___  /
//          \/          \/          \/               \/         \/ /_____/
// Copyright (c) 2011-2015 Scala Team, École polytechnique fédérale de Lausanne
//
// Authors:
//    * Iulian Dragos
//    * Cristian Talau
//    * Vlad Ureche
//
package miniboxing.plugin
package metadata

import scala.collection.mutable

trait MiniboxMethodInfo {
  self: MiniboxInjectComponent =>

  import global._
  import definitions._

  /**
   * While running the `MiniboxInfoTransform` we record information about how
   * the newly created methods should be implemented when reached by the
   * `MiniboxTreeTransformation`.
   */
  object memberSpecializationInfo extends mutable.HashMap[Symbol, MethodInfo] {
    def hasInfo(defn: Tree) = memberSpecializationInfo.this isDefinedAt defn.symbol
  }

  /**
   * This class should be extended by various classes containing information
   * about different types of methods that are created during specialization.
   */
  sealed abstract class MethodInfo {
    def isTag = this match {
      case _: Tag => true
      case _      => false
    }
    private[this] var _isOverride = false
    def isOverride = _isOverride
    def asOverride: this.type = { _isOverride = true; this }
  }

  /**
   * Marker trait for tags
   */
  trait Tag

  /**
   * The symbol with this information needs a body that forwards
   * to `method`.
   *
   * E.g. `apply` forwards to `apply$mcII$sp` in `Function1$mcII$sp`.
   */
  case class ForwardTo(base: Symbol)(overrider: Boolean) extends MethodInfo {
    override def toString =
      if (overrider)
        s"is an override which forwards to the specialized member" // $base"
      else
        s"is a forwarder to the specialized member" // $base"
  }

  /**
   * In the specialized class the function which will have the implementation
   * will be the specialized one and the generic one will forward to it.
   *
   * E.g. `apply$mcII$sp` uses as implementation the body of `apply` which
   * forwards to it. So, `method` will be `apply`.
   */
  case class SpecializedImplementationOf(member: Symbol) extends MethodInfo {
    metadata.templateMembers += member
    override def toString = "is a specialized implementation of " + member
  }

  /**
   * Method to access `field`, in a specialized class should be rewired to the
   * actual field.
   */
  case class FieldAccessor(field: Symbol) extends MethodInfo {
    override def toString = "is a setter or getter for " + field
  }

  /**
   * When the newly introduced symbol is abstract and does not
   * have an implementation at all.
   */
  case object Interface extends MethodInfo {
    override def toString = "is an interface method"
  }

  /**
   * When the newly introduced symbol is abstract and does not
   * have an implementation at all.
   */
  case class TypeTagParam(tparam: Symbol) extends MethodInfo with Tag {
    override def toString = "is a type tag"
  }

  /**
   * When the newly introduced symbol is abstract and does not
   * have an implementation at all.
   */
  case class DeferredTypeTag(tparam: Symbol) extends MethodInfo with Tag {
    override def toString = "is a deferred type tag"
  }

  /**
   * When the newly introduced symbol is abstract and does not
   * have an implementation at all.
   */
  case class DeferredTypeTagImplementation(tparam: Symbol) extends MethodInfo with Tag {
    override def toString = "is the type tag of an inherited trait"
  }
}
