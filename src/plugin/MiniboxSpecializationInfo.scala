package plugin

import scala.collection.mutable

trait MiniboxSpecializationInfo {
  self: MiniboxLogic =>

  import global._

  /**
   * This class should be extended by various classes containing information
   * about different types of methods that are created during specialization.
   */
  sealed class MethodInfo

  /**
   * The symbol with this information needs a body that forwards
   * to `method`.
   *
   * E.g. `apply` forwards to `apply$mcII$sp` in `Function1$mcII$sp`.
   */
  // FIXME: the symbol will have to box its arguments according to
  // their actual types. For this, it has to know the type tags. Include
  // them in the info class
  case class ForwardTo(method: Symbol) extends MethodInfo {
    override def toString = "ForwardTo(" + method.fullName + ")"
  }

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
  case class OverrideOfSpecializedMethod(method: Symbol) extends MethodInfo {
    override def toString = "OverrideOfSpecializedMethod(" + method.fullName + ")"
  }

  /**
   * In the specialized class the function which will have the implementation
   * will be the specialized one and the generic one will forward to it.
   *
   * E.g. `apply$mcII$sp` uses as implementation the body of `apply` which
   * forwards to it. So, `method` will be `apply`.
   */
  case class SpecializedImplementationOf(member: Symbol) extends MethodInfo {
    templateMembers += member
    override def toString = "SpecializedImplementationOf(" + member.fullName + ")"
  }

  /**
   * Method to access `field`, in a specialized class should be rewired to the
   * actual field. 
   */
  case class FieldAccessor(field: Symbol) extends MethodInfo {
    override def toString = "FieldAccessor(" + field.fullName + ")"
  }
  
  /**
   * When the newly introduced symbol is abstract and does not
   * have an implementation at all.
   */
  case class Interface() extends MethodInfo

  /**
   * While running the `MiniboxInfoTransform` we record information about how
   * the newly created methods should be implemented when reached by the
   * `MiniboxTreeTransformation`.
   */
  object memberSpecializationInfo extends mutable.HashMap[Symbol, MethodInfo] {
    def hasInfo(defn: Tree) = memberSpecializationInfo.this isDefinedAt defn.symbol
  }
  
  /**
   * The set of members that provide the template to copy and specialize
   * by the specialized overloads
   */
  val templateMembers = mutable.Set[Symbol]()

  
  /**
   * Every time we create a specialized class (or the interface) we clone 
   * the type parameters from the original class. This mapping records 
   * how the new params correspond to the old ones.
   */
  type ParamMap = Map[Symbol, Symbol]
  
  object ParamMap {
    def apply(oldParams: List[Symbol], newOwner: Symbol): ParamMap = {
      val newParams = oldParams map (_.cloneSymbol(newOwner))
      newParams foreach (_.removeAnnotation(MinispecedClass))
      (oldParams zip newParams).toMap
    }
  }

  case class ClassInfo(sym: Symbol, pmap: ParamMap)
  
  /**
   * `specializedInterface(C)` is the interface `C_interface` extended by all
   * specialized versions of `C`
   */
  val specializedInterface = new mutable.HashMap[Symbol, ClassInfo]
  
  /**
   * `allAnyRefClass(C)` is the class that is not specialized for any parameter
   */
  val allAnyRefClass = new mutable.HashMap[Symbol, ClassInfo]
  
  /**
   * `specializedClass(C)(T1->Long, T2->AnyRef)` gives the info of the specialized
   * version of `C` w.r.t. that environment. 
   */
  val specializedClasses = 
    new mutable.HashMap[Symbol, mutable.HashMap[TypeEnv, ClassInfo]] withDefaultValue (new mutable.HashMap())
    
}

