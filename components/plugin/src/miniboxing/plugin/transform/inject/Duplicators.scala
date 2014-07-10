/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package miniboxing.plugin
package transform
package inject

import scala.tools.nsc._
import scala.tools.nsc.typechecker._
import scala.tools.nsc.symtab.Flags
import scala.collection.mutable

/** Duplicate trees and re-type check them, taking care to replace
 *  and create fresh symbols for new local definitions.
 *
 *  @author  Iulian Dragos
 *  @author  Vlad Ureche
 *
 *  This version of the duplicators in the Scala compiler has been
 *  extensively modified to work with the miniboxing plugin.
 *
 *  @version miniboxing
 */
abstract class Duplicators extends Analyzer with ScalacCrossCompilingLayer {
  import global._
  import definitions.{ AnyRefClass, AnyValClass }

  case class AnnotationAttachment(annots: List[AnnotationInfo])

  def retyped(context: Context, tree: Tree): Tree = {
    resetClassOwners
    (newBodyDuplicator(context)).typed(tree)
  }

  /** Retype the given tree in the given context. Use this method when retyping
   *  a method in a different class. The typer will replace references to the this of
   *  the old class with the new class, and map symbols through the given 'env'. The
   *  environment is a map from type skolems to concrete types (see SpecializedTypes).
   */
  def retyped(context: Context, tree: Tree, oldThis: Symbol, newThis: Symbol, _subst: TypeMap, _deepSubst: TypeMap): Tree = {
    if (oldThis ne newThis) {
      oldClassOwner = oldThis
      newClassOwner = newThis
    } else resetClassOwners

    envSubstitution = _subst
    envDeepSubst = _deepSubst

    newBodyDuplicator(context).typed(tree)
  }

  protected def newBodyDuplicator(context: Context) = new BodyDuplicator(context)

  var indent = 0

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
  private var envDeepSubst: TypeMap = _

  private val invalidSyms: mutable.Map[Symbol, Tree] = perRunCaches.newMap[Symbol, Tree]()

  /** A typer that creates new symbols for all definitions in the given tree
   *  and updates references to them while re-typechecking. All types in the
   *  tree, except for TypeTrees, are erased prior to type checking. TypeTrees
   *  are fixed by substituting invalid symbols for the new ones.
   */
  class BodyDuplicator(_context: Context) extends Typer(_context) {

    class FixInvalidSyms extends TypeMap {

      def apply(tpe: Type): Type = {
        val res = mapOver(tpe)
        // println("type patch: " + tpe + " ==> " + res)
        res
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
                debuglog("fixed by trying harder: "+ sym + "  " + sym1 + "  " + context)
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
          if (newsym == NoSymbol) throw NoSymbolUpdateException
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

    var forceDeep = false
    def withDeepSubst[T](f: => T): T = {
      val forceDeep0 = forceDeep
      forceDeep = true
      val res: T = f
      forceDeep = forceDeep0
      res
    }


    case object NoSymbolUpdateException extends Exception

    /** Fix the given type by replacing invalid symbols with the new ones. */
    def fixType(tpe: Type, deep: Boolean = false): Type = {
      val tpe1 = if (deep) envDeepSubst(tpe) else envSubstitution(tpe)
      val tpe2: Type =
        try {
          (new FixInvalidSyms)(tpe1)
        } catch {
          case NoSymbolUpdateException =>
            global.reporter.warning(oldClassOwner.pos, "[miniboxing-plugin] Recovered from a specialization error. Please report this as a bug!")
            tpe1
        }
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
      // println(s"fixTpe: $tpe ==> $tpe1 (${showRaw(tpe1)}) ==> $tpe2 ==> $tpe3 (deep=$deep)")
      tpe3
    }

    /** Return the new symbol corresponding to `sym`. */
    private def updateSym(sym: Symbol): Symbol = {
      val res = if (invalidSyms.isDefinedAt(sym))
        invalidSyms(sym).symbol
      else
        sym
      // println(s"update sym $sym => $res")
      res
    }

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
            newsym.setInfo(fixType(ldef.symbol.info.cloneInfo(newsym)))
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

          case vdef @ ValDef(mods, name, tpt, rhs) =>
            tpt.setType(
              if (forceDeep)
                envDeepSubst(tpt.tpe)
              else
                envSubstitution(tpt.tpe)
            )
            vdef.symbol = NoSymbol

          case DefDef(_, name, tparams, vparamss, tpt, rhs) =>
            // invalidate parameters
            debuglog("invalidate defdef: " + tree.symbol + "  " + tree.symbol.allOverriddenSymbols + "   " + tree.symbol.owner)
            if (tree.symbol.allOverriddenSymbols.isEmpty) {
              invalidateAll(tparams ::: vparamss.flatten)
              tpt.setType(envSubstitution(tpt.tpe))
            } else {
              withDeepSubst(invalidateAll(tparams))
              withDeepSubst(invalidateAll(vparamss.flatten))
              tpt.setType(envDeepSubst(tpt.tpe))
            }
            tree.symbol = NoSymbol

          case tdef @ TypeDef(_, _, tparams, rhs) =>
            if (!tdef.symbol.annotations.isEmpty) {
              val newAtt = tdef.attachments.update[AnnotationAttachment](AnnotationAttachment(tdef.symbol.annotations))
               tdef.setAttachments(newAtt)
            }
            tdef.symbol = NoSymbol
            invalidateAll(tparams)

          case cdef @ ClassDef(_, _, tparams, impl) =>
            cdef.symbol = NoSymbol
            invalidateAll(tparams)

          case _ =>
            tree.symbol = NoSymbol
        }
      }
    }

    override def typedTypeDef(td: TypeDef): TypeDef = {
      val res = super.typedTypeDef(td)
      td.attachments.get[AnnotationAttachment] match {
        case Some(AnnotationAttachment(annots)) if res.hasSymbolField =>
          for (annot <- annots)
            res.symbol.deSkolemize.addAnnotation(annot)
        case _ =>
      }
      res
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

    def dupldbg(ind: Int, msg: => String): Unit = {
      // println("  " * ind + msg)
    }

    var rewireThis = new scala.util.DynamicVariable(true)

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
    override def typed(tree: Tree, mode: Mode, pt: Type): Tree = {
      // val pt = WildcardType
      val ind = indent
      indent += 1
      dupldbg(ind, " in:  " + tree + ": " + pt + "   " + tree.getClass)

      try {
        debuglog("typing " + tree + ": " + tree.tpe + ", " + tree.getClass)

        val origtreesym = tree.symbol
        if (tree.hasSymbolField && tree.symbol != NoSymbol
            && !tree.symbol.isLabel  // labels cannot be retyped by the type checker as LabelDef has no ValDef/return type trees
            && invalidSyms.isDefinedAt(tree.symbol)) {

          // if possible, shortcut the name-based resolution, which is known to be difficult
          // to get right when shadowed values are used based on their symbol
          val updatedSym = updateSym(tree.symbol)
          if ((updatedSym != null) && (updatedSym != NoSymbol)) {
            if (context.scope.lookupAll(tree.symbol.name).toList != List(updatedSym))
              debuglog("Fixed a duplicators case where pure name-based resolution would crash: " + tree + " in " + context.owner.ownerChain.reverse.map(_.nameString).mkString(".") )
            tree.symbol = updatedSym
          } else {
            // if not possible, resort to name-based resolution :(
            debuglog("removed symbol " + tree.symbol)
            tree.symbol = NoSymbol
          }
        }

        val res = tree match {
          // The point is: any type application should
          // not update the type parameters to @storage
          case TypeApply(sel, tpes) =>
            for (ttree <- tpes)
              ttree.setType(fixType(ttree.tpe, deep = true))
            super.typed(TypeApply(sel, tpes), mode, pt)

          case ttree @ TypeTree() =>
            // log("fixing tpe: " + tree.tpe + " with sym: " + tree.tpe.typeSymbol)
            ttree.setType(fixType(ttree.tpe))
            ttree

          case Block(stats, res) =>
            debuglog("invalidating block")
            invalidateAll(stats)
            invalidate(res)
            dupldbg(ind, " after invalidation: " + tree)
            tree.setType(null)
            super.typed(tree, mode, pt)

          case ClassDef(_, _, _, tmpl @ Template(parents, _, stats)) =>
            // log("invalidating classdef " + tree)
            tmpl.symbol = tree.symbol.newLocalDummy(tree.pos)
            invalidateAll(stats, tree.symbol)
            tree.setType(null)
            super.typed(tree, mode, pt)

          case ddef @ DefDef(_, _, vparamss, _, tpt, rhs) =>
            ddef.setType(null)
            // workaround for SI-7662: https://issues.scala-lang.org/browse/SI-7662
            if (ddef.symbol != null)
              if (ddef.symbol.isStructuralRefinementMember)
                ddef.symbol.setFlag(Flags.PROTECTED)
            super.typed(ddef, mode, pt)

          case vdef @ ValDef(mods, name, tpt, rhs) =>
            // log("vdef fixing tpe: " + tree.tpe + " with sym: " + tree.tpe.typeSymbol + " and " + invalidSyms)
            //if (mods.hasFlag(Flags.LAZY)) vdef.symbol.resetFlag(Flags.MUTABLE) // Martin to Iulian: lazy vars can now appear because they are no longer boxed; Please check that deleting this statement is OK.
            vdef.tpt.setType(fixType(vdef.tpt.tpe))
            vdef.setType(null)
            super.typed(vdef, mode, pt)

          case ldef @ LabelDef(name, params, rhs) =>
            // log("label def: " + ldef)
            // in case the rhs contains any definitions -- TODO: is this necessary?
            invalidate(rhs)
            ldef.setType(null)

            // is this LabelDef generated by tailcalls?
            val isTailLabel = (ldef.params.length >= 1) && (ldef.params.head.name == nme.THIS)

            // the typer does not create the symbols for a LabelDef's params, so unless they were created before we need
            // to do it manually here -- but for the tailcalls-generated labels, ValDefs are created before the LabelDef,
            // so we just need to change the tree to point to the updated symbols
            def newParam(p: Tree): Ident =
              if (isTailLabel)
                Ident(updateSym(p.symbol))
              else {
                val owner = context.owner.enclClass
                val newsym = p.symbol.cloneSymbol(owner)
                Ident(newsym.setInfo(fixType(p.symbol.info)))
              }

            val params1 = params map newParam
            val rhs1 = (new TreeSubstituter(params map (_.symbol), params1) transform rhs) // TODO: duplicate?
            rhs.setType(null)

            super.typed(treeCopy.LabelDef(tree, name, params1, rhs1), mode, pt)

          case Bind(name, _) =>
            // log("bind: " + tree)
            invalidate(tree)
            tree.setType(null)
            super.typed(tree, mode, pt)

          case Ident(_) if tree.symbol.isLabel =>
            debuglog("Ident to labeldef " + tree + " switched to ")
            tree.symbol = updateSym(tree.symbol)
            tree.setType(null)
            super.typed(tree, mode, pt)

          case Ident(_) if (origtreesym ne null) && origtreesym.isLazy =>
            debuglog("Ident to a lazy val " + tree + ", " + tree.symbol + " updated to " + origtreesym)
            tree.symbol = updateSym(origtreesym)
            tree.setType(null)
            super.typed(tree, mode, pt)

          case Select(th @ This(_), sel) if (oldClassOwner ne null) && (th.symbol == oldClassOwner) =>
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

          case EmptyTree =>
            // no need to do anything, in particular, don't set the type to null, EmptyTree.tpe_= asserts
            tree

          case _ =>
            debuglog("Duplicators default case: " + tree.summaryString)
            debuglog(" ---> " + tree)
            if (tree.hasSymbolField && tree.symbol != NoSymbol && (tree.symbol.owner == definitions.AnyClass)) {
              tree.symbol = NoSymbol // maybe we can find a more specific member in a subclass of Any (see AnyVal members, like ==)
            }
            val ntree = castType(tree, pt)
            //println("dupl: " + ntree + ":" + tree.tpe + "  " + pt)
            val res = super.typed(ntree, mode, pt)
            //println(res + " ==> " + res.tpe + "  (" + pt + ")")
            res
        }

        // note: this may force the tree type to be computed, thus producing spurious cyclic reference errors!
        dupldbg(ind, " out: " + res + ": " + res.tpe)
        indent -= 1

        res
      } catch {
        case t: Throwable =>
          dupldbg(ind, " out: <error>")
          indent -= 1
          throw t
      }
    }

  }
}

