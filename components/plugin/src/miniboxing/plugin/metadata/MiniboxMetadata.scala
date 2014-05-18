package miniboxing.plugin
package metadata

import scala.collection.mutable

trait MiniboxMetadata {
  self: MiniboxDuplComponent =>

  import global._
  import definitions._
  import scala.collection.immutable

  // Specialization/normalization info:

  /**  PartialSpec is a binding from type parameters to their representation (Boxed/Miniboxed)
   *   INVARIANT: Regardless of whether the PartialSpec refers to the stem or a variant class,
   *   the parent's type parameters are used. */
  type PartialSpec = immutable.Map[Symbol, SpecInfo]

  sealed trait SpecInfo
  case object Miniboxed extends SpecInfo
  case object Boxed     extends SpecInfo

  object metadata {

    /* Class specialization:
     *
     * (before transformation)
     *                                     +------------------------------------+
     *                                     | class C[@miniboxed T] {            |
     *                                     |   def foo(t: T): T = t             |
     *                                     | }                                  |
     *                                     +------------------------------------+
     *
     * (after transformation)
     *                                     +------------------------------------+ <== this is called *the stem*
     *           specializedStem --------> | trait C[@miniboxed T]              |
     * originalTraitFlag = false --------> |   def foo(t: T): T                 | ---------+  +------ PartialSpec
     *                           +-------> |   def foo_J(...): T @storage       |          |  |         /\
     *      specializedStemClass |         | }                                  |          |  |         /
     *                           |         +------------------------------------+   specializedClasses /
     *                           |             /                            \                |        /
     *                           |            /                              \               V       /
     *              +------------------------------------+        +------------------------------------+
     *              | trait C_L[Tsp] extends C[Tsp] {    |        | trait C_J[Tsp](...) extends C[Tsp]{|  <== these are
     *              |   def foo(t: Tsp): Tsp = t         |        |   def foo(t: Tsp): Tsp = ...       |      called
     *              |   def foo_J(...): Tsp @storage =...|        |   def foo_J(...): Tsp @storage = t |      *specialized
     *              | }                                  |        | }                                  |      variants*
     *              +------------------------------------+        +------------------------------------+
     * Additionally:
     *  for C_L/C_J: T --> Tsp mapping in
     */

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



    // Type parameters:
    /*
     *  in C             in C_L                  in C_J
     * =====================================================
     *   T                Tsp                      Tsp
     *   T                Tsp                 @storage Tsp
     *   |                 ^
     *   +-----------------+
     *    variantParamMap
     */

    /** This mapping records the correspondence from the variants's type parameters to the stem's type parameters
     *  example: Tsp -> T */
    type ParamMap = Map[Symbol, Symbol]

    /** Map from original type parameters to new type parameters */
    val variantParamMap = new mutable.HashMap[Symbol, ParamMap]

    /** A mapping from the variant's type parameters of the original class to the
     *  actual type used in the specialized version to which this environment
     *  correspond. This type may or may not be marked with @storage. */
    type TypeEnv = immutable.Map[Symbol, Type]
    val EmptyTypeEnv: TypeEnv = Map.empty

    /** Type environment of a class: stores the binding from the parent's type parameters to the
     *  specialized variant's type parameters. */
    val variantTypeEnv = new mutable.HashMap[Symbol, TypeEnv]



    // Normalization (local scope):

    /** Map from original type parameters to new type parameters */
    val normalizedParamMap = new mutable.HashMap[Symbol, ParamMap]

    /** Partial normalization corresponding to a normalized method */
    val memberNormalization = new mutable.HashMap[Symbol, PartialSpec]

    /** For each method this keeps a mapping of its normalized variants */
    val normalizedMembers = new mutable.HashMap[Symbol, mutable.HashMap[PartialSpec, Symbol]]

    /** For each method contains the stem method */
    val normalizationStemMember = new mutable.HashMap[Symbol, Symbol]

    /** Type environment of a class: stores the binding from the parent's type parameters to the
     *  specialized variant's type parameters. */
    val normalizedTypeEnv = new mutable.HashMap[Symbol, TypeEnv]


    // Members (local scope, inside a class/trait):

    /** For each method of the original class and each partial specialization
     *  we keep track of the overload specialized for that representation. */
    val specializedMembers = new mutable.HashMap[Symbol, mutable.HashMap[PartialSpec, Symbol]]

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
}

