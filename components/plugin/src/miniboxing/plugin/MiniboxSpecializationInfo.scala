package miniboxing.plugin

import scala.collection.mutable

trait MiniboxSpecializationInfo {
  self: MiniboxLogic with MiniboxingDefinitions =>

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
  case class ForwardTo(method: Symbol, ret: CastInfo, params: List[CastInfo]) extends MethodInfo {
    override def toString = "ForwardTo(" + method.fullName + ")"
  }
  /*
   * We need to record how the parameters and the return value should be casted.
   */
  sealed class CastInfo
  case class CastMiniboxToBox(tag: Symbol) extends CastInfo
  case object CastBoxToMinibox extends CastInfo
  case object NoCast extends CastInfo
  case object AsInstanceOfCast extends CastInfo

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
    override def toString = "SpecializedImplementationOf2(" + member.fullName + ")"
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
      val newParams = oldParams map (p => p.cloneSymbol(newOwner, p.flags, p.name.append("sp")))

      newParams foreach (_.removeAnnotation(MinispecedClass))
      (oldParams zip newParams).toMap
    }
  }

//  /**
//   * `specializedInterface(C)` is the interface `C_interface` extended by all
//   * specialized versions of `C`
//   */
//  val specializedInterface = new mutable.HashMap[Symbol, Symbol]

  /**
   * `specializedClass(C)(T1->Long, T2->AnyRef)` gives the info of the specialized
   * version of `C` w.r.t. that environment.
   */
  val specializedClasses =
    new mutable.HashMap[Symbol, List[Symbol]] withDefaultValue (List())

  /**
   * `baseClass` is the inverse of specializedClass: for each specialized class it
   * indicates the base abstract class all specialized classes extend from
   */
  val baseClass =
    new mutable.HashMap[Symbol, Symbol]

  /**
   * Type environment of a class:
   * Needed by the duplicator to replace the symbols in the old tree.
   */
  val typeEnv = new mutable.HashMap[Symbol, TypeEnv]

  /**
   * Partial specialization corresponding to a class.
   */
  val partialSpec = new mutable.HashMap[Symbol, PartialSpec]

  /**
   * Records for each of the specialized classes the type tag fields corresponding
   * to each type parameter.
   */
  val typeTags = new mutable.HashMap[Symbol, Map[Symbol, Symbol]]

  /**
   * For each method of the original class and each partial specialization
   * we keep track of the overload specialized for that representation.
   */
  val overloads = new mutable.HashMap[Symbol, mutable.HashMap[PartialSpec, Symbol]]

}

