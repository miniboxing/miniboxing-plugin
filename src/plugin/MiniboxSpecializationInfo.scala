package plugin

import scala.collection.immutable.HashMap


/**
 * While running the `MiniboxInfoTransform` we record information about how 
 * the newly created methods should be implemented when reached by the 
 * `MiniboxTreeTransformation`.
 */
object MiniboxSpecializationInfo extends HashMap[Symbol, Info]{
  
  /**
   * The symbol with this information needs a body that forwards
   * to `method` which works with boxed arguments.
   * 
   * E.g. `apply$mcII$sp` forwards to `apply` in `Function1`. 
   */
  // FIXME: the symbol will have to box its arguments according to
  // their actual types. For this, it has to know the type tags. Include
  // them in the info class
  case class ForwardToBoxed(method: Symbol) extends Info
  
  /**
   * The symbol with this information needs a body that forwards
   * to `method` which works with unboxed arguments.
   * 
   * E.g. `apply` forwards to `apply$mcII$sp` in `Function1$mcII$sp`. 
   */
  case class ForwardToUnboxed(method: Symbol) extends Info
  
  /**
   * For the following example:
   *
   *  class IntFun extends Function1[Int, Int] {
   *    def apply(x: Int): Int = ..
   *  }
   *
   *  method `apply` will have type `(Any)Any` and we want in fact to override
   *  `apply$mcII$sp` of type `(Int)Int`, and `apply` will forward to it.
   *  
   *  So, if the symbol is `apply`, the `method` will be `apply$mcII$sp`.
   */
  case class OverrideOfSpecializedMethod(method: Symbol) extends Info 
  
  /**
   * In the specialized class the function which will have the implementation
   * will be the specialized one and the generic one will forward to it.
   * 
   * E.g. `apply$mcII$sp` uses as implementation the body of `apply` which
   * forwards to it. So, `method` will be `apply`.
   */
  case class SpeclializedImplementationOf(method: Symbol) extends Info
  
  
  /**
   * When the newly introduced symbol is abstract and does not
   * have an implementation at all.
   */
  case class Abstract extends Info
}

/**
 * This class should be extended by various classes containing information
 * about different types of methods that are created during specialization. 
 */
sealed class Info
