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
//    * Vlad Ureche
//
package miniboxing.plugin
package metadata

import scala.collection.mutable
import language.implicitConversions

trait MiniboxFlagVersioning extends ScalacVersion {
  self: MiniboxInjectComponent =>

  import global._
  import global.Flag._
  import global.definitions.TailrecClass

  object flagdata {
    /** Miniboxed classes and traits become traits with subclasses/subtraits as specialized variants
     *  This set contains the traits that were transformed. */
    val classStemTraitFlag = mutable.Set.empty[Symbol]

    /** Whether the stem class/trait was marked as abstract before the miniboxing transformation */
    val classStemAbstractFlag = mutable.Set.empty[Symbol]

    /** A list of members that are deferred in the original stem classes */
    val deferredMembers = mutable.Set[/* dummy constructor */ Symbol]()

    /** The stem class constructors that are eliminated by the miniboxing inject phase */
    val stemConstructors = mutable.Set[/* stem class constructors */ Symbol]()

    /** Stem class removed members (see bugs #127 and #166) */
    val stemClassRemovedMembers = perRunCaches.newMap[Symbol, mutable.Set[Symbol]]()
  }

  /** Record which members of the stem are deferred pre-miniboxing */
  def recordDeferredStemMembers(members: List[Symbol]): Unit =
    flagdata.deferredMembers ++= members.filter(_.isDeferred)

  /** ... */
  def removeFieldsFromStem(stemClass: Symbol, stemClassDecls: Scope): Scope = {
    val decls = stemClassDecls.cloneScope
    for (mbr <- decls) {
      if (mbr.isMethod)
        mbr.setFlag(DEFERRED)

      // #166: Protect against synthetic superaccessors, which create duplicate entries in classes:
      //
      // abstract trait B#7862 extends Object#130 with A#8027 {
      //   <superaccessor> <artifact> def super$getStr#16404(): String#232;
      //   override def getStr#15942(): String#232
      // };
      //
      //  abstract trait B_J#17719 extends Object#130 with A_J#17447 with B#7862 {
      //    def B_J|T_TypeTag#17721(): Byte#2468;
      //    <superaccessor> <artifact> def super$getStr#17723(): String#232;
      //    override def getStr#17722(): String#232
      //  };
      //
      //  class C#7877 extends Object#130 with B_J#17719 {
      //    <superaccessor> <artifact> def super$getStr#33169(): String#232 = A_J$class#7887.getStr#33130(C#7877.this); // <= for B
      //    <superaccessor> <artifact> def super$getStr#33171(): String#232 = A_J$class#7887.getStr#33130(C#7877.this); // <= for B_J
      //    override def getStr#33170(): String#232 = B_J$class#7842.getStr#33141(C#7877.this);
      //    def A_J|T_TypeTag#17741(): Byte#2468 = 5;
      //    def B_J|T_TypeTag#17742(): Byte#2468 = 5;
      //    ...
      //  }
      //
      // Solution: Completely move superaccessors to the leaf classes.
      //
      if ((mbr.isTerm && !mbr.isMethod /* field */) || (mbr.isConstructor /* constuctor */) || mbr.isSuperAccessor /* #166 */) {
        if (mbr.isConstructor) flagdata.stemConstructors += mbr
        decls unlink mbr
        if (currentRun.compiles(stemClass))
          flagdata.stemClassRemovedMembers.getOrElseUpdate(stemClass, collection.mutable.Set.empty[Symbol]) += mbr
      }
    }
    // Remove the tailcall notation from members
    decls.foreach(_.removeAnnotation(TailrecClass))
    // This needs to be delayed until trees have been duplicated, else
    // instantiation will fail, since C becomes an abstract class
    if (stemClass.hasFlag(TRAIT))
      flagdata.classStemTraitFlag += stemClass
    if (stemClass.hasFlag(ABSTRACT))
      flagdata.classStemAbstractFlag += stemClass
    stemClass.setFlag(TRAIT | ABSTRACT)

    stemClass.resetFlag(FINAL)
    stemClass.resetFlag(CASE)

    decls
  }

  def preMiniboxingFlags(): Unit = {
    for (sym <- metadata.allStemClasses) {
      if (!flagdata.classStemTraitFlag(sym))
        sym.resetFlag(TRAIT)
      if (!flagdata.classStemAbstractFlag(sym))
        sym.resetFlag(ABSTRACT)
    }

    // add the dummy constructors
    for (ctor <- flagdata.stemConstructors) {
      ctor.owner.info.decls enterIfNew ctor
      ctor.resetFlag(DEFERRED)
    }

    // remove deferred flag from values
    for (sym <- metadata.allStemClasses)
      for (mbr <- sym.info.decls if mbr.isMethod && !flagdata.deferredMembers(mbr))
        mbr.resetFlag(DEFERRED)

  }

  def postMiniboxingFlags(): Unit = {
    for (sym <- metadata.allStemClasses)
      sym.setFlag(ABSTRACT | TRAIT)

    // remove the dummy constructors
    for (ctor <- flagdata.stemConstructors) {
      ctor.owner.info.decls unlink ctor
      ctor setFlag(DEFERRED)
    }

    // remove deferred flag from values
    for (sym <- metadata.allStemClasses)
      for (mbr <- sym.info.decls if mbr.isMethod)
        mbr.setFlag(DEFERRED)
  }

  def refreshTypeHistory(): Unit = {
    // for 2.11, we need to reset flags for miniboxed stem classes, since the type transformer
    // will revisit them (https://github.com/scala/scala/commit/24a0777219d647ec310a0b6da305f619f69950cd)
    if (scalaBinaryVersion == "2.11") { // TODO: Refine the condition, now it's too coarse
      preMiniboxingFlags()
      for (stem <- metadata.allStemClasses)
        // force the updated info on the symbol
        afterMiniboxInject(stem.info)
    }
  }
}