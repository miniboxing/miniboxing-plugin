//
//     _____   .__         .__ ___.                    .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_ |__    ____  ___  ___|__|  ____     ____
//   /  \ /  \ |  | /    \ |  | | __ \  /  _ \ \  \/  /|  | /    \   / ___\
//  /    Y    \|  ||   |  \|  | | \_\ \(  <_> ) >    < |  ||   |  \ / /_/  >
//  \____|__  /|__||___|  /|__| |___  / \____/ /__/\_ \|__||___|  / \___  /
//          \/          \/          \/               \/         \/ /_____/
// Copyright (c) 2012-2014 Scala Team, École polytechnique fédérale de Lausanne
//
// Authors:
//    * Vlad Ureche
//
package miniboxing.plugin
package metadata

import scala.collection.mutable

trait MiniboxMetadata {
  self: MiniboxInjectComponent =>

  import global._
  import definitions._
  import scala.collection.immutable

  trait metadata {

    val miniboxedMemberFlag = mutable.Set.empty[Symbol]
    val miniboxedTParamFlag = mutable.Set.empty[Symbol]

    /* Class specialization:
     *
     * (before transformation)
     *                                +------------------------------------+
     *                                | class C[@miniboxed T] {            |
     *                                |   def foo(t: T): T = t             |
     *                                | }                                  |
     *                                +------------------------------------+
     *
     * (after transformation)
     *                                +------------------------------------+ <== this is called *the stem*
     *                                | trait C[@miniboxed T]              |
 classStemTraitFlag = false --------> |   def foo(t: T): T                 | ---------+   +------ PartialSpec
     *                      +-------> |   def foo_J(...): T @storage       |          |   |        /\
     *            classStem |         | }                                  |          |   |        /
     *                      |         +------------------------------------+     classOverloads   /
     *                      |             /                            \                |        / classSpecialization
     *                      |            /                              \               V       /
     *         +------------------------------------+        +------------------------------------+
     *         | class C_L[Tsp] extends C[Tsp] {    |        | class C_J[Tsp](...) extends C[Tsp]{|  <== these are
     *         |   def foo(t: Tsp): Tsp = t         |        |   def foo(t: Tsp): Tsp = ...       |      called
     *         |   def foo_J(...): Tsp @storage =...|        |   def foo_J(...): Tsp @storage = t |      *specialized
     *         | }                                  |        | }                                  |      variants*
     *         +------------------------------------+        +------------------------------------+
     *
     * Additionally:
     *  for C_L/C_J: T --> Tsp mapping in
     */

    /** Miniboxed classes and traits become traits with subclasses/subtraits as specialized variants
     *  This set contains the traits that were transformed. */
    val classStemTraitFlag = mutable.Set.empty[Symbol]

    val classOverloads = new mutable.HashMap[Symbol, mutable.HashMap[PartialSpec, Symbol]]

    val classSpecialization = new mutable.HashMap[Symbol, PartialSpec]

    protected val classStem = new mutable.HashMap[Symbol, Symbol]



    // Members (local scope, inside a class/trait):

    val memberOverloads = new mutable.HashMap[Symbol, mutable.HashMap[PartialSpec, Symbol]]

    val memberSpecialization = new mutable.HashMap[Symbol, PartialSpec]

    protected val memberStem = new mutable.HashMap[Symbol, Symbol]

    val variantMemberStem = new mutable.HashMap[Symbol, Symbol]



    // Normalization (local scope):

    /** Partial normalization corresponding to a normalized method */
    val normalSpecialization = new mutable.HashMap[Symbol, PartialSpec]

    /** For each method this keeps a mapping of its normalized variants */
    val normalOverloads = new mutable.HashMap[Symbol, mutable.HashMap[PartialSpec, Symbol]]

    /** For each method contains the stem method */
    protected val normalStem = new mutable.HashMap[Symbol, Symbol]



    // Type tags (local scope, inside a class/trait):

    /** Records for each of the specialized classes the tag field to type parameter
     *  correspondence. These are local type tags, used in all members. */
    val globalTypeTags = new mutable.HashMap[Symbol, mutable.Map[Symbol, Symbol]]

    /** Records for each of the specialized classes the tag field to type parameter
     *  correspondence. These are local type tags, used in each member. */
    val localTypeTags = new mutable.HashMap[Symbol, mutable.Map[Symbol, Symbol]]

    /** TODO */
    val normalTypeTags = new mutable.HashMap[Symbol, mutable.Map[Symbol, Symbol]]

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


  /** Contains the metadata and accessors */
  object metadata extends metadata {

    // Accessors:

    // Classes:
    def setClassStem(variant: Symbol, stem: Symbol) = {
      assert(variant.isClass, s"Not a class: ${variant.defString}")
      assert(stem.isClass, s"Not a class: ${stem.defString}")
      classStem += variant -> stem
    }

    def getClassStem(variant: Symbol) = {
//      assert(variant.isClass || variant.isModule || variant.isPackage || variant.isPackageObject, s"Not a class/module/package/package object: ${variant.defString}")
      classStem.getOrElse(variant, NoSymbol)
    }

    def isClassStem(clazz: Symbol) =
      getClassStem(clazz) == clazz

    def allStemClasses: Set[Symbol] =
      classStem.values.toSet

    def getClassLocalSpecialization(sym: Symbol): PartialSpec = {
      val stem = getClassStem(sym)
      val sym_tparams = sym.typeParams
      val stem_tparams = stem.typeParams
      val localTParam = (stem_tparams zip sym_tparams).toMap
      val normalization = metadata.classSpecialization.getOrElse(sym, Map.empty)
      normalization.map({ case (tparam, spec) => (localTParam(tparam), spec)})
    }


    // Members:
    def setMemberStem(variant: Symbol, stem: Symbol) = {
      assert(variant.isMethod, s"Not a method: ${variant.defString}")
      assert(stem.isMethod, s"Not a method: ${stem.defString}")
      memberStem += variant -> stem
    }

    def getMemberStem(variant: Symbol) = {
      assert(variant.isMethod || (variant.isTerm && !variant.isMethod), s"Not a method/field: ${variant.defString}")
      classStem.getOrElse(variant, NoSymbol)
    }

    def isMemberStem(variant: Symbol) =
      getMemberStem(variant) == variant

    def getMemberLocalSpecialization(sym: Symbol): PartialSpec = {
      val stem = getMemberStem(sym)
      val sym_tparams = sym.typeParams
      val stem_tparams = stem.typeParams
      val localTParam = (stem_tparams zip sym_tparams).toMap
      val normalization = metadata.memberSpecialization.getOrElse(sym, Map.empty)
      normalization.map({ case (tparam, spec) => (localTParam(tparam), spec)})
    }

    def memberHasOverloads(stem: Symbol): Boolean =
      memberOverloads.get(stem).map(_.size).getOrElse(1) > 1

    // Normalizations:
    def setNormalStem(variant: Symbol, stem: Symbol) = {
      assert(variant.isMethod || (variant.isTerm && !variant.isMethod), s"Not a method/field: ${variant.defString}")
      assert(stem.isMethod || (stem.isTerm && !stem.isMethod), s"Not a method/field: ${stem.defString}")
      normalStem += variant -> stem
    }

    def getNormalStem(variant: Symbol) = {
      assert(variant.isMethod || (variant.isTerm && !variant.isMethod), s"Not a method/field: ${variant.defString}")
      normalStem.getOrElse(variant, NoSymbol)
    }

    def isNormalStem(variant: Symbol) =
      getNormalStem(variant) == variant

    def getNormalLocalSpecialization(sym: Symbol): PartialSpec = {
      val stem = getNormalStem(sym)
      val sym_tparams = sym.typeParams
      val stem_tparams = stem.typeParams
      val localTParam = (stem_tparams zip sym_tparams).toMap
      val normalization = metadata.normalSpecialization.getOrElse(sym, Map.empty)
      normalization.map({ case (tparam, spec) => (localTParam(tparam), spec)})
    }

    def memberHasNormalizations(stem: Symbol): Boolean =
      normalOverloads.get(stem).map(_.size).getOrElse(1) > 1
  }
}

