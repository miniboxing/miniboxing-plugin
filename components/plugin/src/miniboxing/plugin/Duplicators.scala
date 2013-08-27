/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package miniboxing.plugin

import scala.tools.nsc._
import scala.tools.nsc.typechecker._
import scala.tools.nsc.symtab.Flags
import scala.collection.{ mutable, immutable }

/** Duplicate trees and re-type check them, taking care to replace
 *  and create fresh symbols for new local definitions.
 *
 *  @author  Iulian Dragos
 *  @version 1.0
 */
abstract class Duplicators extends Analyzer {
  import global._
  import definitions.{ AnyRefClass, AnyValClass }

  def retyped(context: Context, tree: Tree): Tree = {
    resetClassOwners
    (newBodyDuplicator(context)).typed(tree)
  }

  /** Retype the given tree in the given context. Use this method when retyping
   *  a method in a different class. The typer will replace references to the this of
   *  the old class with the new class, and map symbols through the given 'env'. The
   *  environment is a map from type skolems to concrete types (see SpecializedTypes).
   */
  def retyped(context: Context, tree: Tree, oldThis: Symbol, newThis: Symbol, _envSubstitution: TypeMap): Tree = {
    if (oldThis ne newThis) {
      oldClassOwner = oldThis
      newClassOwner = newThis
    } else resetClassOwners

    envSubstitution = _envSubstitution

    newBodyDuplicator(context).typed(tree)
  }

  protected def newBodyDuplicator(context: Context) = new BodyDuplicator(context)

  /** Return the special typer for duplicate method bodies. */
  override def newTyper(context: Context): Typer =
    newBodyDuplicator(context)

  private def resetClassOwners() {
    oldClassOwner = null
    newClassOwner = null
  }

  private var oldClassOwner: Symbol = _
  private var newClassOwner: Symbol = _
  private var envSubstitution: TypeMap = _

  private val invalidSyms: mutable.Map[Symbol, Tree] = perRunCaches.newMap[Symbol, Tree]()

  /** A typer that creates new symbols for all definitions in the given tree
   *  and updates references to them while re-typechecking. All types in the
   *  tree, except for TypeTrees, are erased prior to type checking. TypeTrees
   *  are fixed by substituting invalid symbols for the new ones.
   */
  class BodyDuplicator(_context: Context) extends Typer(_context) {

    class FixInvalidSyms extends TypeMap {

      def apply(tpe: Type): Type = {
        mapOver(tpe)
      }

      override def mapOver(tpe: Type): Type = tpe match {
        case TypeRef(NoPrefix, sym, args) if sym.isTypeParameterOrSkolem =>
          var sym1 = context.scope.lookup(sym.name)
          if (sym1 eq NoSymbol) {
            // try harder (look in outer scopes)
            // with virtpatmat, this can happen when the sym is referenced in the scope of a LabelDef but is defined in the scope of an outer DefDef (e.g., in AbstractPartialFunction's andThen)
            BodyDuplicator.super.silent(_.typedType(Ident(sym.name))) match {
              case SilentResultValue(t) =>
                sym1 = t.symbol
                debuglog("fixed by trying harder: "+(sym, sym1, context))
              case _ =>
            }
          }
//          assert(sym1 ne NoSymbol, tpe)
          if ((sym1 ne NoSymbol) && (sym1 ne sym)) {
            debuglog("fixing " + sym + " -> " + sym1)
            typeRef(NoPrefix, sym1, mapOverArgs(args, sym1.typeParams))
          } else super.mapOver(tpe)

        case TypeRef(pre, sym, args) =>
          val newsym = updateSym(sym)
          if (newsym ne sym) {
            debuglog("fixing " + sym + " -> " + newsym)
            typeRef(mapOver(pre), newsym, mapOverArgs(args, newsym.typeParams))
          } else
            super.mapOver(tpe)

        case SingleType(pre, sym) =>
          val sym1 = updateSym(sym)
          if (sym1 ne sym) {
            debuglog("fixing " + sym + " -> " + sym1)
            singleType(mapOver(pre), sym1)
          } else
            super.mapOver(tpe)

        case ThisType(sym) =>
          val sym1 = updateSym(sym)
          if (sym1 ne sym) {
            debuglog("fixing " + sym + " -> " + sym1)
            ThisType(sym1)
          } else
            super.mapOver(tpe)

        case _ =>
          super.mapOver(tpe)
      }
    }

    /** Fix the given type by replacing invalid symbols with the new ones. */
    def fixType(tpe: Type): Type = {
      val tpe1 = envSubstitution(tpe)
      val tpe2: Type = (new FixInvalidSyms)(tpe1)
      // known problem: asSeenFrom on an abstract type produced by creating new syms
      // from symbols bound in existential types crashes the AsSeenFromMap
      //  error: T#24681 in trait Complex#7190 cannot be instantiated from miniboxing#27.tests#7186.compile#7188.Complex_J#23156[Tsp#23157]
      //  unhandled exception while transforming mb_spire_2.scala
      //  error: uncaught exception during compilation: scala.reflect.internal.FatalError
      //  error: scala.reflect.internal.FatalError:
      //         while compiling: mb_spire_2.scala
      //            during phase: minibox
      //         library version: version 2.10.3-20130708-144415-62405227dd
      //        compiler version: version 2.10.3-20130708-163611-504b5f3b15
      //      reconstructed args: -no-specialization -Xprint:minibox -uniqid -Ycheck:minibox -bootclasspath /home/sun/workspace/dev/miniboxing-plugin//components/runtime/target/scala-2.10/miniboxing-runtime_2.10-0.1-SNAPSHOT.jar:/home/sun/workspace/dev/miniboxing-plugin//components/plugin/target/scala-2.10/miniboxing-plugin_2.10-0.1-SNAPSHOT.jar -Xplugin:/home/sun/workspace/dev/miniboxing-plugin//components/plugin/target/scala-2.10/miniboxing-plugin_2.10-0.1-SNAPSHOT.jar -P:minibox:hijack -P:minibox:log
      //
      //      last tree to typer: term canEqual
      //                  symbol: method canEqual#23178 in class Complex_J#23156 (flags: <method> <synthetic> <triedcooking>)
      //   symbol definition: def canEqual#23178(x$1#23179: Any#3411): Boolean#1396
      //       symbol owners: method canEqual#23178 -> class Complex_J#23156 -> package compile#7188
      //      context owners: method canEqual#23178 -> class Complex_J#23156 -> package compile#7188
      //
      //  == Enclosing template or block ==
      //
      //  Apply( // final def $isInstanceOf#5273[T0#5274](): Boolean#1396 in class Object#1737
      //    TypeApply( // final def $isInstanceOf#5273[T0#5274](): Boolean#1396 in class Object#1737
      //      "x$1"."$isInstanceOf" // final def $isInstanceOf#5273[T0#5274](): Boolean#1396 in class Object#1737, tree.tpe=[T0#5274]()Boolean#1396
      //      <tpt> // tree.tpe=miniboxing#27.tests#7186.compile#7188.Complex#7190[_]
      //    )
      //    Nil
      //  )
      //
      //  T#24681 in trait Complex#7190 cannot be instantiated from miniboxing#27.tests#7186.compile#7188.Complex_J#23156[Tsp#23157]
      //    at scala.reflect.internal.SymbolTable.abort(SymbolTable.scala:49)
      //    at scala.tools.nsc.Global.abort(Global.scala:254)
      //    at scala.reflect.internal.Types$AsSeenFromMap.throwError$1(Types.scala:4579)
      //    at scala.reflect.internal.Types$AsSeenFromMap.instParam$1(Types.scala:4599)
      //    at scala.reflect.internal.Types$AsSeenFromMap.toInstance$1(Types.scala:4619)
      //    at scala.reflect.internal.Types$AsSeenFromMap.apply(Types.scala:4637)
      //    at scala.reflect.internal.Types$AsSeenFromMap.apply(Types.scala:4501)
      //    at scala.collection.immutable.List.loop$1(List.scala:170)
      //    at scala.collection.immutable.List.mapConserve(List.scala:186)
      //    at scala.reflect.internal.Types$TypeMap.mapOver(Types.scala:4185)
      //    at scala.reflect.internal.Types$AsSeenFromMap.apply(Types.scala:4639)
      //    at scala.reflect.internal.Types$TypeMap.mapOver(Types.scala:4233)
      //    at scala.reflect.internal.Types$AsSeenFromMap.apply(Types.scala:4639)
      //    at scala.reflect.internal.Types$Type.asSeenFrom(Types.scala:754)
      //    at miniboxing.plugin.Duplicators$BodyDuplicator.fixType(Duplicators.scala:143)
      //    at miniboxing.plugin.Duplicators$BodyDuplicator.typed(Duplicators.scala:241)
      //    at scala.tools.nsc.typechecker.Typers$Typer.typedType(Typers.scala:5730)
      //    at scala.tools.nsc.typechecker.Typers$Typer.typedHigherKindedType(Typers.scala:5737)
      //    at scala.tools.nsc.typechecker.Typers$Typer$$anonfun$112.apply(Typers.scala:5429)
      //    at scala.tools.nsc.typechecker.Typers$Typer$$anonfun$112.apply(Typers.scala:5429)
      //    at scala.reflect.internal.Types$class.map2Conserve(Types.scala:6416)
      //    at scala.reflect.internal.SymbolTable.map2Conserve(SymbolTable.scala:13)
      //    at scala.tools.nsc.typechecker.Typers$Typer.typedTypeApply$1(Typers.scala:5427)
      //    at scala.tools.nsc.typechecker.Typers$Typer.typed1(Typers.scala:5533)
      //    at scala.tools.nsc.typechecker.Typers$Typer.typed(Typers.scala:5603)
      //    at miniboxing.plugin.Duplicators$BodyDuplicator.typed(Duplicators.scala:400)
      //    at scala.tools.nsc.typechecker.Typers$Typer$$anonfun$92.apply(Typers.scala:4569)
      //    at scala.tools.nsc.typechecker.Typers$Typer$$anonfun$92.apply(Typers.scala:4569)
      //    at scala.tools.nsc.typechecker.Typers$Typer.silent(Typers.scala:727)
      //    at scala.tools.nsc.typechecker.Typers$Typer.normalTypedApply$1(Typers.scala:4569)
      //    at scala.tools.nsc.typechecker.Typers$Typer.typedApply$1(Typers.scala:4620)
      //    at scala.tools.nsc.typechecker.Typers$Typer.typed1(Typers.scala:5525)
      //    at scala.tools.nsc.typechecker.Typers$Typer.typed(Typers.scala:5603)
      //    at miniboxing.plugin.Duplicators$BodyDuplicator.typed(Duplicators.scala:400)
      //    at scala.tools.nsc.typechecker.Typers$Typer.transformedOrTyped(Typers.scala:5806)
      //    at scala.tools.nsc.typechecker.Typers$Typer.typedDefDef(Typers.scala:2254)
      //    at scala.tools.nsc.typechecker.Typers$Typer.typed1(Typers.scala:5530)
      //    at scala.tools.nsc.typechecker.Typers$Typer.typed(Typers.scala:5603)
      //    at miniboxing.plugin.Duplicators$BodyDuplicator.typed(Duplicators.scala:261)
      //    at scala.tools.nsc.typechecker.Typers$Typer.typed(Typers.scala:5665)
      //    at miniboxing.plugin.Duplicators.retyped(Duplicators.scala:42)
      val tpe3 = try {
        if (newClassOwner ne null)
          tpe2.asSeenFrom(newClassOwner.thisType, oldClassOwner)
        else tpe2
      } catch { case e: Throwable => tpe2}
      tpe3
    }

    /** Return the new symbol corresponding to `sym`. */
    private def updateSym(sym: Symbol): Symbol =
      if (invalidSyms.isDefinedAt(sym))
        invalidSyms(sym).symbol
      else
        sym

    private def invalidate(tree: Tree, owner: Symbol = NoSymbol) {
      debuglog("attempting to invalidate " + tree.symbol)
      if (tree.isDef && tree.symbol != NoSymbol) {
        debuglog("invalid " + tree.symbol)
        invalidSyms(tree.symbol) = tree

        tree match {
          case ldef @ LabelDef(name, params, rhs) =>
            debuglog("LabelDef " + name + " sym.info: " + ldef.symbol.info)
            invalidSyms(ldef.symbol) = ldef
          //          breakIf(true, this, ldef, context)
            val newsym = ldef.symbol.cloneSymbol(context.owner)
//            println("FROM: " + ldef.symbol.defString)
//            println("TO:   " + newsym.defString)
            newsym.setInfo(fixType(ldef.symbol.info))
            ldef.symbol = newsym
            debuglog("newsym: " + newsym + " info: " + newsym.info)

          case vdef @ ValDef(mods, name, _, rhs) if mods.hasFlag(Flags.LAZY) =>
            debuglog("ValDef " + name + " sym.info: " + vdef.symbol.info)
            invalidSyms(vdef.symbol) = vdef
            val newowner = if (owner != NoSymbol) owner else context.owner
            val newsym = vdef.symbol.cloneSymbol(newowner)
            newsym.setInfo(fixType(vdef.symbol.info))
            vdef.symbol = newsym
            debuglog("newsym: " + newsym + " info: " + newsym.info + ", owner: " + newsym.owner + ", " + newsym.owner.isClass)
            if (newsym.owner.isClass) newsym.owner.info.decls enter newsym

          case DefDef(_, name, tparams, vparamss, _, rhs) =>
            // invalidate parameters
            invalidateAll(tparams ::: vparamss.flatten)
            tree.symbol = NoSymbol

          case _ =>
            tree.symbol = NoSymbol
        }
      }
    }

    private def invalidateAll(stats: List[Tree], owner: Symbol = NoSymbol) {
      stats.foreach(invalidate(_, owner))
    }

    private def inspectTpe(tpe: Type) = {
      tpe match {
        case MethodType(_, res) =>
          res + ", " + res.bounds.hi + ", " + (res.bounds.hi match {
            case TypeRef(_, _, args) if (args.length > 0) => args(0) + ", " + args(0).bounds.hi
            case _ => "non-tref: " + res.bounds.hi.getClass
          })
        case _ =>
      }
    }

    /** Optionally cast this tree into some other type, if required.
     *  Unless overridden, just returns the tree.
     */
    def castType(tree: Tree, pt: Type): Tree = tree

    /** Special typer method for re-type checking trees. It expects a typed tree.
     *  Returns a typed tree that has fresh symbols for all definitions in the original tree.
     *
     *  Each definition tree is visited and its symbol added to the invalidSyms map (except LabelDefs),
     *  then cleared (forcing the namer to create fresh symbols).
     *  All invalid symbols found in trees are cleared (except for LabelDefs), forcing the
     *  typechecker to look for fresh ones in the context.
     *
     *  Type trees are typed by substituting old symbols for new ones (@see fixType).
     *
     *  LabelDefs are not typable from trees alone, unless they have the type ()Unit. Therefore,
     *  their symbols are recreated ad-hoc and their types are fixed inline, instead of letting the
     *  namer/typer handle them, or Idents that refer to them.
     */
    override def typed(tree: Tree, mode: Int, pt: Type): Tree = {
      debuglog("typing " + tree + ": " + tree.tpe + ", " + tree.getClass)
      val origtreesym = tree.symbol
      if (tree.hasSymbol && tree.symbol != NoSymbol
          && !tree.symbol.isLabel  // labels cannot be retyped by the type checker as LabelDef has no ValDef/return type trees
          && invalidSyms.isDefinedAt(tree.symbol)) {
        debuglog("removed symbol " + tree.symbol)
        tree.symbol = NoSymbol
      }

      tree match {
        case ttree @ TypeTree() =>
          // log("fixing tpe: " + tree.tpe + " with sym: " + tree.tpe.typeSymbol)
          ttree.tpe = fixType(ttree.tpe)
          ttree

        case Block(stats, res) =>
          debuglog("invalidating block")
          invalidateAll(stats)
          invalidate(res)
          tree.tpe = null
          super.typed(tree, mode, pt)

        case ClassDef(_, _, _, tmpl @ Template(parents, _, stats)) =>
          // log("invalidating classdef " + tree)
          tmpl.symbol = tree.symbol.newLocalDummy(tree.pos)
          invalidateAll(stats, tree.symbol)
          tree.tpe = null
          super.typed(tree, mode, pt)

        case ddef @ DefDef(_, _, _, _, tpt, rhs) =>
          ddef.tpt.tpe = fixType(ddef.tpt.tpe)
          ddef.tpe = null
          // workaround for SI-7662: https://issues.scala-lang.org/browse/SI-7662
          if (ddef.symbol != null)
            if (ddef.symbol.isStructuralRefinementMember)
              ddef.symbol.setFlag(Flags.PROTECTED)
          super.typed(ddef, mode, pt)

        case vdef @ ValDef(mods, name, tpt, rhs) =>
          // log("vdef fixing tpe: " + tree.tpe + " with sym: " + tree.tpe.typeSymbol + " and " + invalidSyms)
          //if (mods.hasFlag(Flags.LAZY)) vdef.symbol.resetFlag(Flags.MUTABLE) // Martin to Iulian: lazy vars can now appear because they are no longer boxed; Please check that deleting this statement is OK.
          vdef.tpt.tpe = fixType(vdef.tpt.tpe)
          vdef.tpe = null
          super.typed(vdef, mode, pt)

        case ldef @ LabelDef(name, params, rhs) =>
          // log("label def: " + ldef)
          // in case the rhs contains any definitions -- TODO: is this necessary?
          invalidate(rhs)
          ldef.tpe = null

          // is this LabelDef generated by tailcalls?
          val isTailLabel = (ldef.params.length >= 1) && (ldef.params.head.name == nme.THIS)

          // the typer does not create the symbols for a LabelDef's params, so unless they were created before we need
          // to do it manually here -- but for the tailcalls-generated labels, ValDefs are created before the LabelDef,
          // so we just need to change the tree to point to the updated symbols
          def newParam(p: Tree): Ident =
            if (isTailLabel)
              Ident(updateSym(p.symbol))
            else {
              val newsym = p.symbol.cloneSymbol(context.owner) // TODO owner?
              Ident(newsym.setInfo(fixType(p.symbol.info)))
            }

          val params1 = params map newParam
          val rhs1 = (new TreeSubstituter(params map (_.symbol), params1) transform rhs) // TODO: duplicate?
          rhs1.tpe = null

          super.typed(treeCopy.LabelDef(tree, name, params1, rhs1), mode, pt)

        case Bind(name, _) =>
          // log("bind: " + tree)
          invalidate(tree)
          tree.tpe = null
          super.typed(tree, mode, pt)

        case Ident(_) if tree.symbol.isLabel =>
          debuglog("Ident to labeldef " + tree + " switched to ")
          tree.symbol = updateSym(tree.symbol)
          tree.tpe = null
          super.typed(tree, mode, pt)

        case Ident(_) if (origtreesym ne null) && origtreesym.isLazy =>
          debuglog("Ident to a lazy val " + tree + ", " + tree.symbol + " updated to " + origtreesym)
          tree.symbol = updateSym(origtreesym)
          tree.tpe = null
          super.typed(tree, mode, pt)

        case Select(th @ This(_), sel) if (oldClassOwner ne null) && (th.symbol == oldClassOwner) =>
          if (tree.symbol.name != nme.CONSTRUCTOR) {
            // We use the symbol name instead of the tree name because the symbol
            // may have been name mangled, rendering the tree name obsolete.
            // ...but you can't just do a Select on a name because if the symbol is
            // overloaded, you will crash in the backend.
            val memberByName  = newClassOwner.thisType.member(tree.symbol.name)
            def nameSelection = Select(This(newClassOwner), tree.symbol.name)
            val newTree = (
              if (memberByName.isOverloaded) {
                // Find the types of the overload alternatives as seen in the new class,
                // and filter the list down to those which match the old type (after
                // fixing the old type so it is seen as if from the new class.)
                val typeInNewClass = fixType(oldClassOwner.info memberType tree.symbol)
                val alts           = memberByName.alternatives
                val memberTypes    = alts map (newClassOwner.info memberType _)
                val memberString   = memberByName.defString
                alts zip memberTypes filter (_._2 =:= typeInNewClass) match {
                  case ((alt, tpe)) :: Nil =>
                    log(s"Arrested overloaded type in Duplicators, narrowing to ${alt.defStringSeenAs(tpe)}\n  Overload was: $memberString")
                    Select(This(newClassOwner), alt)
                  case _ =>
                    log(s"Could not disambiguate $memberString in Duplicators. Attempting name-based selection, but this may not end well...")
                    nameSelection
                }
              }
              else nameSelection
            )
            super.typed(atPos(tree.pos)(newTree), mode, pt)
          } else {
            // since rewiring constructors is complicated business, we'd rather leave the constructor call
            // from the original class and then have it rewired later. Also, since retyping will trigger the
            // This(oldClass) => This(newClass) rewiring, we just leave the tree as it was before
            tree
          }

        case This(_) if (oldClassOwner ne null) && (tree.symbol == oldClassOwner) =>
//          val tree1 = Typed(This(newClassOwner), TypeTree(fixType(tree.tpe.widen)))
          // log("selection on this: " + tree)
          val tree1 = This(newClassOwner)
          // log("tree1: " + tree1)
          debuglog("mapped " + tree + " to " + tree1)
          super.typedPos(tree.pos, mode, pt)(tree1)

        case This(_) =>
          debuglog("selection on this, plain: " + tree)
          tree.symbol = updateSym(tree.symbol)
          val ntree = castType(tree, pt)
          val tree1 = super.typed(ntree, mode, pt)
          // log("plain this typed to: " + tree1)
          tree1
/* no longer needed, because Super now contains a This(...)
        case Super(qual, mix) if (oldClassOwner ne null) && (tree.symbol == oldClassOwner) =>
          val tree1 = Super(qual, mix)
          log("changed " + tree + " to " + tree1)
          super.typed(atPos(tree.pos)(tree1))
*/
        case Match(scrut, cases) =>
          val scrut1   = typed(scrut, EXPRmode | BYVALmode, WildcardType)
          val scrutTpe = scrut1.tpe.widen
          val cases1 = {
            if (scrutTpe.isFinalType) cases filter {
              case CaseDef(Bind(_, pat @ Typed(_, tpt)), EmptyTree, body) =>
                // the typed pattern is not incompatible with the scrutinee type
                scrutTpe matchesPattern fixType(tpt.tpe)
              case CaseDef(Typed(_, tpt), EmptyTree, body) =>
                // the typed pattern is not incompatible with the scrutinee type
                scrutTpe matchesPattern fixType(tpt.tpe)
              case _ => true
            }
            // Without this, AnyRef specializations crash on patterns like
            //   case _: Boolean => ...
            // Not at all sure this is safe.
            else if (scrutTpe <:< AnyRefClass.tpe)
              cases filterNot (_.pat.tpe <:< AnyValClass.tpe)
            else
              cases
          }

          super.typed(atPos(tree.pos)(Match(scrut, cases1)), mode, pt)

        case EmptyTree =>
          // no need to do anything, in particular, don't set the type to null, EmptyTree.tpe_= asserts
          tree

        case _ =>
          debuglog("Duplicators default case: " + tree.summaryString)
          debuglog(" ---> " + tree)
          if (tree.hasSymbol && tree.symbol != NoSymbol && (tree.symbol.owner == definitions.AnyClass)) {
            tree.symbol = NoSymbol // maybe we can find a more specific member in a subclass of Any (see AnyVal members, like ==)
          }
          val ntree = castType(tree, pt)
          val res = super.typed(ntree, mode, pt)
          res
      }
    }

  }
}

