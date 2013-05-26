/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package miniboxing.plugin

import scala.tools.nsc.symtab.Flags
import scala.collection.{ mutable, immutable }
import scala.tools.nsc._
import scala.tools.nsc.typechecker._

/** Duplicate trees and re-type check them, taking care to replace
 *  and create fresh symbols for new local definitions.
 *
 *  @author  Iulian Dragos
 *  @version 1.0
 *
 *  Vlad Ureche: I have to branch duplicators so I can make changes to it to suit miniboxing
 *   -- extending Analyzer just to override newTyper is a major PITA. Need to redesign this away
 */
abstract class Duplicators extends Analyzer {
  //val global: miniboxing.global.type
  val miniboxing: MiniboxComponent { val global: Duplicators.this.global.type }

  import miniboxing._
  import global._
  import definitions.{ AnyRefClass, AnyValClass }

  var indent = 0;

  // This is the only reason we inherit from Analyzer. TODO: Maybe there's a better way to do it...
  override def newTyper(context: Context): Typer = newBodyDuplicator(context).asInstanceOf[Typer]

  def retyped(context: Context, tree: Tree): Tree = {
    resetClassOwners
    (newBodyDuplicator(context)).typed(tree)
  }

  /** Retype the given tree in the given context. Use this method when retyping
   *  a method in a different class. The typer will replace references to the this of
   *  the old class with the new class, and map symbols through the given 'env'. The
   *  environment is a map from type skolems to concrete types (see SpecializedTypes).
   */
  def retyped(context: Context, tree: Tree, oldThis: Symbol, newThis: Symbol, env: MiniboxingTypeEnv[TypeEnv]): Tree = {
    if (oldThis ne newThis) {
      oldClassOwner = oldThis
      newClassOwner = newThis
    } else resetClassOwners

    envSubstitution = new SubstSkolemsTypeMap(env.deepEnv.keysIterator.toList, env.deepEnv.valuesIterator.toList)
    debuglog("retyped with env: " + env)
    newBodyDuplicator(context).typed(tree)
  }

  def newBodyDuplicator(context: Context) = new BodyDuplicator(context)

  def retypedMethod(context: Context, tree: DefDef, oldThis: Symbol, newThis: Symbol, env: MiniboxingTypeEnv[TypeEnv]): Tree = {
    this.env = env
    this.envSubstitution = new SubstSkolemsTypeMap(env.deepEnv.keysIterator.toList, env.deepEnv.valuesIterator.toList)
    (newBodyDuplicator(context)).retypedMethodMB(tree, oldThis, newThis)
  }

  private def resetClassOwners() {
    oldClassOwner = null
    newClassOwner = null
  }

  private var oldClassOwner: Symbol = _
  private var newClassOwner: Symbol = _
  private var interfaceClass: Symbol = _
  private var envSubstitution: SubstTypeMap = _
  private var env: MiniboxingTypeEnv[TypeEnv] = _

  private class SubstSkolemsTypeMap(from: List[Symbol], to: List[Type]) extends SubstTypeMap(from, to) {
    protected override def matches(sym1: Symbol, sym2: Symbol) =
      if (sym2.isTypeSkolem) sym2.deSkolemize eq sym1
      else sym1 eq sym2
  }

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
            // with virtpatmat, this can happen when the sym is referenced in the scope of a LabelDef but is defined in
            // the scope of an outer DefDef (e.g., in AbstractPartialFunction's andThen)
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
      val tpe3 = if (newClassOwner ne null) {
        tpe2.asSeenFrom(newClassOwner.thisType, oldClassOwner)
      } else tpe2
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
            // println(t.symbol.id)

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

    def retypedMethod(ddef: DefDef, oldThis: Symbol, newThis: Symbol): Tree = {
      oldClassOwner = oldThis
      newClassOwner = newThis
      invalidateAll(ddef.tparams)
      mforeach(ddef.vparamss) { vdef =>
        invalidate(vdef)
        vdef.tpe = null
      }
      ddef.symbol = NoSymbol
      enterSym(context, ddef)
      debuglog("remapping this of " + oldClassOwner + " to " + newClassOwner)
      typed(ddef)
    }

    /** Miniboxing-specific retypedMethod */
    def retypedMethodMB(ddef: DefDef, oldThis: Symbol, newThis: Symbol): Tree = {
      oldClassOwner = oldThis
      newClassOwner = newThis
      invalidateAll(ddef.tparams)

      // TODO: Invalidate method parameters

//      ddef.symbol = NoSymbol
//      enterSym(context, ddef)
      //logTree(ddef.symbol.toString, ddef)
      debuglog("remapping this of " + oldClassOwner + " to " + newClassOwner)
      typed(ddef)
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
      indent += 1
      debug("  " * indent + " typing " + tree + ": " + tree.tpe + ", " + tree.getClass)
//      if (indent == 1) {
//        debug("\n\n\n")
//        logTree("duplicator:", tree)
//      }
      debug("  " * indent + " in:  " + tree)


      val origtreesym = tree.symbol
      if (tree.hasSymbol && tree.symbol != NoSymbol
          && !tree.symbol.isLabel  // labels cannot be retyped by the type checker as LabelDef has no ValDef/return type trees
          && invalidSyms.isDefinedAt(tree.symbol)) {
        debug("removed symbol " + tree.symbol)
        tree.symbol = NoSymbol
      }

      val result = tree match {
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
              val newsym = p.symbol.cloneSymbol //(context.owner) // TODO owner?
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
          // TODO: Test with lazy vals if they work correctly
          tree.tpe = null
          super.typed(tree, mode, pt)

//        case Ident(_) if {
//          // condition: the tree is of type T and the type is bound to the
//              println("    => " + tree.tpe.typeSymbol)
//              println("    => " + updateSym(tree.tpe.typeSymbol))
//              println("    => " + fixType(tree.tpe))
////
////          val retyped = super.typed(Ident(updateSym(tree.symbol)))
////          val cond2 = (retyped.tpe.normalize.typeSymbol == global.definitions.LongClass)
////        } =>
////          // box the

        case Select(th @ This(_), sel) if (oldClassOwner ne null) && (th.symbol == oldClassOwner) =>
          // log("selection on this, no type ascription required")
          // we use the symbol name instead of the tree name because the symbol may have been
          // name mangled, rendering the tree name obsolete
          // log(tree)
          val t = super.typed(atPos(tree.pos)(Select(This(newClassOwner), tree.symbol.name)), mode, pt)
          // log("typed to: " + t + "; tpe = " + t.tpe + "; " + inspectTpe(t.tpe))
          t

        case This(_) if (oldClassOwner ne null) && (tree.symbol == oldClassOwner) =>
//          val tree1 = Typed(This(newClassOwner), TypeTree(fixType(tree.tpe.widen)))
          // log("selection on this: " + tree)
          val tree1 = This(newClassOwner)
          // log("tree1: " + tree1)
          debuglog("mapped " + tree + " to " + tree1)
          super.typed(atPos(tree.pos)(tree1), mode, pt)

        case This(_) =>
          debuglog("selection on this, plain: " + tree)
          tree.symbol = updateSym(tree.symbol)
          val ntree = castType(tree, pt)
          val tree1 = super.typed(ntree, mode, pt)
          // log("plain this typed to: " + tree1)
          tree1

        case EmptyTree =>
          // no need to do anything, in particular, don't set the type to null, EmptyTree.tpe_= asserts
          tree

        case _ =>
          debug("Duplicators default case: " + tree.summaryString)
          //debuglog(" ---> " + tree)
//          val newTree = tree match {
//            case Ident(_) if ((tree.tpe != null) && !(envSubstitution(tree.tpe) =:= tree.tpe)) =>
//              // we might need to insert miniboxing calls around the identifier for example if it's a
//              println("    => " + tree.tpe.typeSymbol)
//              println("    => " + updateSym(tree.tpe.typeSymbol))
//              println("    => " + fixType(tree.tpe))
//
////              val newType = TypeTree()
////              newType.tpe = fixType(tree.tpe)
////              val value = Ident(updateSym(origtreesym))
////              val tag = Ident(miniboxing.typeTags(newClassOwner)(tree.tpe.typeSymbol))
////              val boxed = Apply(TypeApply(Ident(minibox2box), List(newType)), List(value, tag))
////              atPos(tree.pos)(boxed)
//              tree
//            case _ =>
//              tree
//          }

          if (tree.hasSymbol && tree.symbol != NoSymbol && (tree.symbol.owner == definitions.AnyClass)) {
            tree.symbol = NoSymbol // maybe we can find a more specific member in a subclass of Any (see AnyVal members, like ==)
          }

          // we are translating the generic class into the specialized class, but the two have nothing in common (the
          // specialized class does not inherit from the generic class)
          // This leads to the following "funny" scenario: we have a value of abstract type T, which is updated to type
          // Tp, according to the minixboxing rules -- but the tree node expects T (tree.tpe = T) -- so the typer will
          // attempt to adapt Tp to T. In specialization, this works, as the specialized class exends the generic class,
          // but in miniboxing it crashes, as there's no relation between T and Tp.
          // Solution: reset tree.tpe to null. Note that since the node inside has been erased of types, although using
          // fixType would yield the correct type, it would prevent the typer from recusing inside the tree node and
          // resolving symbols
          tree.tpe = null

          val ntree = castType(tree, pt)
          //logTree("default case: ", ntree.asInstanceOf[miniboxing.global.Tree])
          //debuglog(ntree.summaryString)
          super.typed(ntree, mode, pt)
      }

      debug("  " * indent + " out: " + result + " : " + result.tpe)
      indent -= 1
      result
    }

  }
}

