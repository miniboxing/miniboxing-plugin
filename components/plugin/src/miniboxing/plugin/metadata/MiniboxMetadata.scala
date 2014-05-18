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
    /* TODO: private[this] */ val classStemTraitFlag = mutable.Set.empty[Symbol]

    /* TODO: private[this] */  val classOverloads =
      new mutable.HashMap[Symbol, mutable.HashMap[PartialSpec, Symbol]] withDefaultValue (mutable.HashMap())

    /* TODO: private[this] */  val classSpecialization = new mutable.HashMap[Symbol, PartialSpec]

    private[this] val classStem = new mutable.HashMap[Symbol, Symbol]

    def setClassStem(variant: Symbol, stem: Symbol) = {
      assert(variant.isClass, s"Not a class: ${variant.defString}")
      assert(stem.isClass, s"Not a class: ${stem.defString}")
      classStem += variant -> stem
    }

    def getClassStem(variant: Symbol) = {
      assert(variant.isClass, s"Not a class: ${variant.defString}")
      classStem.getOrElse(variant, NoSymbol)
    }

    def isClassStem(clazz: Symbol) =
      getClassStem(clazz) == clazz

    // Members (local scope, inside a class/trait):

    /* TODO: private[this] */  val memberOverloads =
      new mutable.HashMap[Symbol, mutable.HashMap[PartialSpec, Symbol]] withDefaultValue (mutable.HashMap())

    /* TODO: private[this] */  val memberSpecialization = new mutable.HashMap[Symbol, PartialSpec]

    private[this] val memberStem = new mutable.HashMap[Symbol, Symbol]

    def setMemberStem(variant: Symbol, stem: Symbol) = {
      assert(variant.isMethod, s"Not a method: ${variant.defString}")
      assert(stem.isMethod, s"Not a method: ${stem.defString}")
      memberStem += variant -> stem
    }

    def getMemberStem(variant: Symbol) = {
      assert(variant.isMethod, s"Not a method: ${variant.defString}")
      classStem.getOrElse(variant, NoSymbol)
    }

    def isMemberStem(clazz: Symbol) =
      getMemberStem(clazz) == clazz

    // Normalization (local scope):

    /** Partial normalization corresponding to a normalized method */
    /* TODO: private[this] */ val normalSpecialization = new mutable.HashMap[Symbol, PartialSpec]

    /** For each method this keeps a mapping of its normalized variants */
    /* TODO: private[this] */ val normalOverloads =
      new mutable.HashMap[Symbol, mutable.HashMap[PartialSpec, Symbol]] withDefaultValue (mutable.HashMap())

    /** For each method contains the stem method */
    private[this] val normalStem = new mutable.HashMap[Symbol, Symbol]

    def setNormalStem(variant: Symbol, stem: Symbol) = {
      assert(variant.isMethod, s"Not a method: ${variant.defString}")
      assert(stem.isMethod, s"Not a method: ${stem.defString}")
      normalStem += variant -> stem
    }

    def getNormalStem(variant: Symbol) = {
      assert(variant.isMethod, s"Not a method: ${variant.defString}")
      normalStem.getOrElse(variant, NoSymbol)
    }

    def isNormalStem(clazz: Symbol) =
      getNormalStem(clazz) == clazz


    // Type tags (local scope, inside a class/trait):

    /** Records for each of the specialized classes the tag field to type parameter
     *  correspondence. These are local type tags, used in all members. */
    /* TODO: private[this] */ val globalTypeTags =
      new mutable.HashMap[Symbol, mutable.Map[Symbol, Symbol]] withDefaultValue (mutable.HashMap())

    /** Records for each of the specialized classes the tag field to type parameter
     *  correspondence. These are local type tags, used in each member. */
    /* TODO: private[this] */ val localTypeTags =
      new mutable.HashMap[Symbol, mutable.Map[Symbol, Symbol]] withDefaultValue (mutable.HashMap())

    /** TODO */
    /* TODO: private[this] */ val normalTypeTags =
      new mutable.HashMap[Symbol, mutable.Map[Symbol, Symbol]] withDefaultValue (mutable.HashMap())

    /** A list of members that represent type tags *inherited* from traits -- unlike type tags in a class,
     *  which are fields, these are methods which the inheriting class overrides. */
    /* TODO: private[this] */ val inheritedDeferredTypeTags =
      new mutable.HashMap[Symbol, mutable.Map[Symbol, Symbol]] withDefaultValue (mutable.HashMap())

    /** A list of members that represent a trait's *own type tags* -- not the ones inherited, but the ones
     *  corresponding to its type parameters. Keep in mind that several deferred type tags may ultimately correspond
     *  to the same type parameter, since the methods have different names in different inherited traits.  */
    /* TODO: private[this] */ val primaryDeferredTypeTags =
      new mutable.HashMap[Symbol, mutable.Map[Symbol, Symbol]] withDefaultValue (mutable.HashMap())



    // Very specific things:

    /** A list of dummy constructors necessary to satisfy the duplicator */
    /* TODO: private[this] */ val dummyConstructors = mutable.Set[/* dummy constructor */ Symbol]()

    /** The set of members that provide the template to copy and specialize by the specialized overloads */
    /* TODO: private[this] */ val templateMembers = mutable.Set[Symbol]()
  }
}

