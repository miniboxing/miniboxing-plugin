package miniboxing.plugin
package metadata

import scala.collection.mutable

trait MiniboxMetadata {
  self: MiniboxDuplComponent =>

  import global._
  import definitions._
  import scala.collection.immutable

  /**
   * before transformation:
   *                                     +------------------------------------+
   *                                     | class C[@miniboxed T] {            |
   *                                     |   def foo(t: T): T = t             |
   *                                     | }                                  |
   *                                     +------------------------------------+
   * after transformation:
   *                                     +------------------------------------+
   *           specializedStem --------> | trait C[@miniboxed T]              | ---------+  +------ PartialSpec
   * originalTraitFlag = false --------> |   def foo(t: T): T                 |          |  |          /\
   *                           +-------> |   def foo_J(...): T @storage       |          |  |          /
   *      specializedStemClass |         | }                                  |   specializedClasses  /
   *                           |         +------------------------------------+            |         /
   *                           |             /                            \                |        /
   *                           |            /                              \               V       /
   *              +------------------------------------+        +------------------------------------+
   *              | trait C_L[Tsp] extends C[Tsp] {    |        | trait C_J[Tsp](...) extends C[Tsp]{|
   *              |   def foo(t: Tsp): Tsp = t         |        |   def foo(t: Tsp): Tsp = ...       |
   *              |   def foo_J(...): Tsp @storage =...|        |   def foo_J(...): Tsp @storage = t |
   *              | }                                  |        | }                                  |
   *              +------------------------------------+        +------------------------------------+
   *
   */

  /** A `TypeEnv` maps each type parameter of the original class to the
   *  actual type used in the specialized version to which this environment
   *  correspond. This type may or may not be marked with @storage. */
  type TypeEnv = immutable.Map[Symbol, Type]
  val EmptyTypeEnv: TypeEnv = Map.empty

  /**
   * A `PartialSpec` provides us information about the representation used
   * for values of a type parameter: either `Boxed` (as AnyRef) or
   * `Miniboxed` (as Long).
   */
  sealed trait SpecInfo
  case object Miniboxed extends SpecInfo
  case object Boxed extends SpecInfo

  /**
   *  PartialSpec is a binding from the parent's type parameters to whether
   *  they are miniboxed or boxed. INVARIANT: This always refers to the
   *  parent's type parameters.
   */
  type PartialSpec = immutable.Map[Symbol, SpecInfo]

  /**
   * Every time we create a specialized class (or the interface) we clone
   * the type parameters from the original class. This mapping records
   * how the new params correspond to the old ones.
   */
  type ParamMap = Map[Symbol, Symbol]

  object ParamMap {
    def apply(oldParams: List[Symbol], newOwner: Symbol): ParamMap = {
      def newName(p: Symbol): Name = p.name.append("sp")
      val newParams = oldParams map (p => p.cloneSymbol(newOwner, p.flags, newName(p)))

      // Update references to old type parameters to the new type parameters
      // See https://github.com/miniboxing/miniboxing-plugin/issues/36 for details.
      newParams.map(_.modifyInfo(info => info.substituteSymbols(oldParams, newParams)))

      newParams foreach (p => { p.removeAnnotation(MinispecClass); p.removeAnnotation(SpecializedClass) })
      (oldParams zip newParams).toMap
    }
  }

  /** Miniboxed classes and traits become traits with subclasses/subtraits as specialized variants
   *  This set contains the traits that were transformed. */
  val originalTraitFlag = mutable.Set.empty[Symbol]

  /** This set contains the classes and traits that were transformed to top traits. */
  val specializedStem = mutable.Set.empty[Symbol]

  /** Indicates the specialized variant corresponding to a type parameter specialization. */
  val specializedClasses =
    new mutable.HashMap[Symbol, mutable.HashMap[PartialSpec, Symbol]] withDefaultValue (mutable.HashMap())

  /** The inverse of specializedClass: for each specialized class it
   *  indicates the base abstract class all specialized classes extend from */
  val specializedStemClass = new mutable.HashMap[Symbol, Symbol]

  /** Partial specialization corresponding to a class. */
  val classSpecialization = new mutable.HashMap[Symbol, PartialSpec]

  // TODO: Stopped here

  /** Type environment of a class: stores the binding from the parent's type parameters to the
   *  specialized variant's type parameters. */
  val typeEnv = new mutable.HashMap[Symbol, TypeEnv]

  // TODO: Remove
  /** Map from original type parameters to new type parameters */
  val typeParamMap = new mutable.HashMap[Symbol, ParamMap]


  // Normalization (local scope):

  /** Partial specialization corresponding to a method's normalization. */
  val normalSpec = new mutable.HashMap[Symbol, PartialSpec]

  /** For each method this keeps a mapping of further normalized methods that
   *  can be used as redirects and optimized terms */
  val normalizations = new mutable.HashMap[Symbol, mutable.HashMap[PartialSpec, Symbol]]

  val normalizationStemMember = new mutable.HashMap[Symbol, Symbol]

  // Members (local scope, inside a class/trait):

  /** For each method of the original class and each partial specialization
   *  we keep track of the overload specialized for that representation. */
  val overloads = new mutable.HashMap[Symbol, mutable.HashMap[PartialSpec, Symbol]]

  /** Which of the members are base (do not take any type tags)
   *  TODO: Transform into a set */
  val specializationStemMember = new mutable.HashMap[Symbol, Symbol]



  // Type tags (local scope, inside a class/trait):

  /** Records for each of the specialized classes the tag field to type parameter
   *  correspondence. These are local type tags, used in all members. */
  val globalTypeTags = new mutable.HashMap[Symbol, Map[Symbol, Symbol]]

  /** Records for each of the specialized classes the tag field to type parameter
   *  correspondence. These are local type tags, used in each member. */
  val localTypeTags = new mutable.HashMap[Symbol, Map[Symbol, Symbol]]

  /** A list of members that represent type tags *inherited* from traits -- unlike type tags in a class,
   *  which are fields, these are methods which the inheriting class overrides. */
  val inheritedDeferredTypeTags = new mutable.HashMap[Symbol, mutable.Map[Symbol, Symbol]]

  /** A list of members that represent a trait's *own type tags* -- not the ones inherited, but the ones
   *  corresponding to its type parameters. Keep in mind that several deferred type tags may ultimately correspond
   *  to the same type parameter, since the methods have different names in different inherited traits.  */
  val primaryDeferredTypeTags = new mutable.HashMap[Symbol, mutable.Map[Symbol, Symbol]]



  // Very specific things:

  /** A list of dummy constructors necessary to satisfy the duplicator */
  val dummyConstructors = mutable.Set[/* dummy constructor */ Symbol]()

  /** The set of members that provide the template to copy and specialize by the specialized overloads */
  val templateMembers = mutable.Set[Symbol]()

}

