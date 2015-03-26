/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package miniboxing.plugin
package transform
package minibox
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
abstract class Duplicators extends TweakedDuplicator with ScalacCrossCompilingLayer with ScalacVersion {
  import global._
  import definitions.{ AnyRefClass, AnyValClass, AnyClass, AnyTpe, AnyRefTpe, AnyValTpe }

  def postTransform(onwer: Symbol, tree: Tree): Tree
  def suboptimalCodeWarning(pos: Position, msg: String, isSymbolGenericAnnotated: Boolean = false): Unit
  def flag_create_local_specs: Boolean

  case class AnnotationAttachment(annots: List[AnnotationInfo])

//  def retyped(context: Context, tree: Tree): Tree = {
//    resetClassOwners
//    (newBodyDuplicator(context)).typed(tree)
//  }

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

    val owner = context.owner
    postTransform(owner, newBodyDuplicator(context).typed(tree))
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

  // Bug #184: `case object` expands into `case class` and a getter
  // but when the entire thing is duplicated, the `case class` wants
  // to create the companion object, which lives in the same term
  // namespace as the def Apple(), leading to the clash seen in
  //
  //   https://github.com/miniboxing/miniboxing-plugin/issues/184
  //
  // ```
  //    case object Apple extends Object with Product with Serializable {
  //      ...
  //    }
  //    @volatile var Apple$module: Apple.type = _
  //    case <stable> def Apple(): Apple.type = {
  //      Apple$module = new Apple.type();
  //      Apple$module
  //    }
  // ```
  // solution: don't add the companion object to the scope at all :)
  class DerivedFromObject(val module: Symbol)
  def tweakedEnsureCompanionObject(namer: Namer, cdef: ClassDef, creator: ClassDef => Tree = companionModuleDef(_)): Option[Symbol] =
    if (cdef.hasAttachment[DerivedFromObject])
      Some(NoSymbol)
    else
      None


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
      val tpe2 =
        try {
          if (newClassOwner ne null)
            tpe1.asSeenFrom(newClassOwner.thisType, oldClassOwner)
          else
            tpe1
        } catch {
          case e: Throwable =>
            global.reporter.warning(oldClassOwner.pos, "[miniboxing-plugin] Recovered from a specialization error. Please report this as a bug! (tpe2)")
            tpe1
        }
      val tpe3: Type =
        try {
          (new FixInvalidSyms)(tpe2)
        } catch {
          case NoSymbolUpdateException =>
            global.reporter.warning(oldClassOwner.pos, "[miniboxing-plugin] Recovered from a specialization error. Please report this as a bug! (tpe3)")
            tpe2
        }
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

    private def updateInvalidSym(t1: Tree, t2: Tree): Unit = {
      invalidSyms.find({case (sym, tree) => tree == t1}) match {
        case Some((sym, _)) => invalidSyms(sym) = t2
        case None           => global.reporter.warning(t1.pos, "[internal miniboxing plugin error] could not update symbol for definition: " + t1)
      }
    }

    private def invalidate(tree: Tree, owner: Symbol = NoSymbol): List[Tree] = {
      debuglog("attempting to invalidate " + tree.symbol)
      if ((tree.isDef && tree.symbol != NoSymbol) || tree.isInstanceOf[Try]) {
        debuglog("invalid " + tree.symbol)
        invalidSyms(tree.symbol) = tree

        tree match {
          case _: DefTree =>
            if (!tree.symbol.annotations.isEmpty) {
              val newAtt = tree.attachments.update[AnnotationAttachment](AnnotationAttachment(tree.symbol.annotations))
               tree.setAttachments(newAtt)
            }
          case _ => // do nothing
        }

        tree match {
          case ldef @ LabelDef(name, params, rhs) =>
            debuglog("LabelDef " + name + " sym.info: " + ldef.symbol.info)
            invalidSyms(ldef.symbol) = ldef
          //          breakIf(true, this, ldef, context)
            val newsym = ldef.symbol.cloneSymbol(context.owner)
            newsym.setInfo(fixType(ldef.symbol.info.cloneInfo(newsym)))
            ldef.symbol = newsym
            debuglog("newsym: " + newsym + " info: " + newsym.info)
            List(ldef)

          case vdef @ ValDef(mods, name, _, rhs) if mods.hasFlag(Flags.LAZY) =>
            debuglog("ValDef " + name + " sym.info: " + vdef.symbol.info)
            invalidSyms(vdef.symbol) = vdef
            val newowner = if (owner != NoSymbol) owner else context.owner
            val newsym = vdef.symbol.cloneSymbol(newowner)
            newsym.setInfo(fixType(vdef.symbol.info))
            vdef.symbol = newsym
            debuglog("newsym: " + newsym + " info: " + newsym.info + ", owner: " + newsym.owner + ", " + newsym.owner.isClass)
            if (newsym.owner.isClass) newsym.owner.info.decls enter newsym
            List(vdef)

          case vdef @ ValDef(mods, name, tpt, rhs) =>
            tpt.setType(
              if (forceDeep)
                envDeepSubst(tpt.tpe)
              else
                envSubstitution(tpt.tpe)
            )
            vdef.symbol = NoSymbol
            List(vdef)

          case DefDef(_, name, tparams, vparamss, tpt, rhs) =>

            def changeSignature = {
              // TODO: Fix this side-effecting call:
              invalidateAll(tparams ::: vparamss.flatten)
              tpt.setType(envSubstitution(tpt.tpe))
            }

            def keepSignature = {
              // TODO: Fix this side-effecting call:
              withDeepSubst(invalidateAll(tparams ::: vparamss.flatten))
              tpt.setType(envDeepSubst(tpt.tpe))
            }

            // Bugs #138 and #148: Specializing nested methods and patching object-oriented
            // behavior using bridges.
            //
            // The correct approach is to only modify a signature when there is no way the
            // program can observe the signature update. Detection can be done in 4 ways:
            //
            //  1. the member overrides another member -- the modified signature doesn't
            //     override the original signature anymore
            //  2. the member can be overridden later (and called with super) -- the modified
            //     signature can't be called using super, as its signature changed and can't
            //     the new signature doesn't override anything
            //  3. **using structural types** - we must make sure the member is not included
            //     in a structural type, otherwise the promised structural type is not valid
            //     anymore
            //  4. through reflection or method handles: well, we might have a flag that says
            //     preserve signatures intact, although we've ran this pedestrian over long
            //     ago, when we decided to split up classes...
            //
            // Another way to say this is: "Hotul neprins, negustor cinstit"
            // (translation from Romanian: "An uncaught scammer is an honest trader")
            // that's our method with its signarture :D
            val symbol = tree.symbol
            val owner  = symbol.owner
            val ownerIsClass = owner.isClass || owner.isTrait || owner.isModuleOrModuleClass
            val cantBeOverridden = !ownerIsClass || owner.isAnonOrRefinementClass || symbol.isPrivate
            val doesntOverrideOthers = !ownerIsClass || symbol.allOverriddenSymbols.isEmpty
            val isStructuralRefinement = ownerIsClass && symbol.isStructuralRefinementMember

            val tsyms = symbol.tpe.finalResultType.typeSymbol :: symbol.paramss.flatten.map(_.tpe.typeSymbol)
            val tpes = tsyms.map(_.tpeHK)
            val mod1 = tpes.map(envSubstitution)
            val mod2 = tpes.map(envDeepSubst)
            val shouldWarn =
              (mod1 zip mod2) exists {
                case (tp1, tp2) => tp1.annotations.map(_.symbol) != tp2.annotations.map(_.symbol)
              }

            if (flag_create_local_specs)
              if (cantBeOverridden && doesntOverrideOthers && !isStructuralRefinement)
                changeSignature
              else {
                keepSignature
                if (shouldWarn) {
                  lazy val GenericClass = rootMirror.getRequiredClass("scala.generic")
                  (doesntOverrideOthers, cantBeOverridden, isStructuralRefinement) match {
                    case (false, _, _) =>
//                      This error will appear later, when the class is extending something else:
//                      suboptimalCodeWarning(tree.pos,
//                          "This member cannot have its signature minibox-transformed since it overrides the following " +
//                          "class/trait members: \n" +
//                          symbol.allOverriddenSymbols.map(sym => " * " + sym.defString + " from " + sym.owner).mkString("\n") +
//                          "\nTo benefit from a miniboxed version of the method, mark the type parameters of the above " +
//                          "classes/traits with the @miniboxed annotation.", symbol hasAnnotation GenericClass)
                    case (_, false, _) =>
                      suboptimalCodeWarning(tree.pos,
                          "The " + symbol + " cannot have its signature minibox-transformed as it becomes part of " +
                          symbol.owner + ", which allows outer code to call and/or override it. If you don't use it " +
                          "outside this " + symbol.owner.kindString + ", you can make it private and miniboxing will " +
                          "be allowed to specialize it:", symbol hasAnnotation GenericClass)
                    case (_, _, true) =>
                      suboptimalCodeWarning(tree.pos,
                          "The " + symbol + " cannot have its signature minibox-transformed as it becomes part of a type, " +
                          "which allows outer code to call it. If you don't use it outside, you can allow miniboxing " +
                          "to transform the signature by making the member protected or private:", symbol hasAnnotation GenericClass)
                    case (_, _, _) =>
                      // here just to satisfy exhaustivity, since the if condition protects us from getting here
                  }
                }
              }
            else
              keepSignature

            tree.symbol = NoSymbol
            List(tree)

          case tdef @ TypeDef(_, _, tparams, rhs) =>
            tdef.symbol = NoSymbol
            // TODO: Fix this side-effecting call:
            invalidateAll(tparams)
            List(tdef)

          case cdef @ ClassDef(_, _, tparams, impl) =>

            // Bug #184: mark `case class` which is derived from object
            // (search for 184 in this file for more details)
            if (cdef.symbol.isModuleClass) {
              cdef.updateAttachment(new DerivedFromObject(cdef.symbol.sourceModule))
              if (cdef.symbol.sourceModule.isCase && scalaBinaryVersion == "2.10")
                global.reporter.error(cdef.symbol.pos, "You ran into a Scala compiler limitation that prevents the miniboxing " +
                                                       "plugin from correctly transforming your code. The bug can only be " +
                                                       "worked around in Scala 2.11. Please upgrade your compiler version. " +
                                                       "See https://github.com/miniboxing/miniboxing-plugin/issues/184 for " +
                                                       "more information (and a possible workaround). Thanks and sorry!")
            }

            cdef.symbol = NoSymbol
            // TODO: Fix this side-effecting call:
            invalidateAll(tparams)

            List(cdef)

          case Try(_, cases, _) =>
            cases foreach {
              case CaseDef(bnd: Bind, _, _) =>
                invalidSyms(bnd.symbol) = bnd
                bnd.symbol = NoSymbol
              case _ =>
            }
            List(tree)

          case _ =>
            tree.symbol = NoSymbol
            List(tree)
        }
      } else
        List(tree)
    }

    def addAnnotations[T <: Tree, U <: Tree](orig: T, f: T => U): U = {
      val tree: U = f(orig)
      orig.attachments.get[AnnotationAttachment] match {
        case Some(AnnotationAttachment(annots)) if tree.hasSymbolField =>
          for (annot <- annots)
            tree.symbol.deSkolemize.addAnnotation(annot)
        case _ =>
      }
      tree
    }

    // Add annotations back to all DefTrees
    override def typedTypeDef(td: TypeDef)     = addAnnotations(td, super.typedTypeDef)
    override def typedDefDef(dd: DefDef)       = addAnnotations(dd, super.typedDefDef)
    override def typedValDef(vd: ValDef)       = addAnnotations(vd, super.typedValDef)
    override def typedClassDef(cd: ClassDef)   = addAnnotations(cd, super.typedClassDef)
    override def typedLabelDef(md: LabelDef)   = addAnnotations(md, super.typedLabelDef)
    override def typedModuleDef(md: ModuleDef) = addAnnotations(md, super.typedModuleDef)

    private def invalidateAll(stats: List[Tree], owner: Symbol = NoSymbol): List[Tree] = {
      stats.flatMap(invalidate(_, owner))
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

    def dupldbg(ind: Int, msg: => String): Unit = {
//       println("  " * ind + msg)
    }

    var rewireThis = new scala.util.DynamicVariable(true)

    private def allAndLast(list: List[Tree]): (List[Tree], Tree) =
      list match {
        case Nil => ???
        case List(sth) => (Nil, sth)
        case other => (other.take(other.length-1), other.last)
      }

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
            val (stats2, res2) = allAndLast(invalidateAll(stats) ::: invalidate(res))
            dupldbg(ind, " after invalidation: " + tree)
            super.typed(Block(stats2, res2), mode, pt)

          case ClassDef(mods, name, targs, tmpl @ Template(parents, self, stats)) =>
            // log("invalidating classdef " + tree)
            tmpl.symbol = tree.symbol.newLocalDummy(tree.pos)
            val stats2 = invalidateAll(stats, tree.symbol)
            val tree2 = treeCopy.ClassDef(tree, mods, name, targs, treeCopy.Template(tmpl, parents, self, stats2).setSymbol(tmpl.symbol)).setSymbol(tree.symbol)
            tree2.setType(null)
            updateInvalidSym(tree, tree2)
            // Bug #184: protect against module classes with no module symbol attached
            // for which modClass.typeOfThis.typeSymbol == NoSymbol, breaking the
            // parent checks in typedTemplate.
            val sym = tree2.symbol
            if (tree.hasAttachment[DerivedFromObject]) {
              val module = updateSym(tree.attachments.get[DerivedFromObject].get.module)
              sym.sourceModule = module
              module.asInstanceOf[TermSymbol].referenced = sym
            }
            super.typed(tree2, mode, pt)

          case ddef @ DefDef(_, _, vparamss, _, tpt, rhs) =>
            ddef.setType(null)
            // workaround for SI-7662: https://issues.scala-lang.org/browse/SI-7662
            // NOTE: Only works for Scala 2.11. For Scala 2.10, we need to override
            // the structural type check and there we make the defs protected:
            if (ddef.symbol != null)
              if (ddef.symbol.isStructuralRefinementMember) {
                ddef.symbol.setFlag(Flags.PROTECTED)
                ddef.symbol.setFlag(Flags.notPROTECTED)
              }
            super.typed(ddef, mode, pt)

          case vdef @ ValDef(mods, name, tpt, rhs) =>
            // log("vdef fixing tpe: " + tree.tpe + " with sym: " + tree.tpe.typeSymbol + " and " + invalidSyms)
            //if (mods.hasFlag(Flags.LAZY)) vdef.symbol.resetFlag(Flags.MUTABLE) // Martin to Iulian: lazy vars can now appear because they are no longer boxed; Please check that deleting this statement is OK.
            vdef.tpt.setType(fixType(vdef.tpt.tpe))
            vdef.setType(null)
            // bug #157: PRIVATE and PROTECTED should never be active at the same time:
            if (vdef.symbol.isPrivate && vdef.symbol.isProtected)
              vdef.symbol.resetFlag(Flags.PROTECTED)
            super.typed(vdef, mode, pt)

          case ldef @ LabelDef(name, params, rhs) =>
            // log("label def: " + ldef)
            // in case the rhs contains any definitions -- TODO: is this necessary?
            val rhs1 = allAndLast(invalidate(rhs)) match {
                case (Nil, last) => last
                case (all, last) => Block(all, last)
              }
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
            val rhs2 = (new TreeSubstituter(params map (_.symbol), params1) transform rhs1) // TODO: duplicate?
            rhs.setType(null)

            val ldef2 = treeCopy.LabelDef(tree, name, params1, rhs2)
            updateInvalidSym(ldef, ldef2)
            super.typed(ldef2, mode, pt)

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
            tree.setType(null)
            val tree1 = super.typed(tree, mode, pt)
            // log("plain this typed to: " + tree1)
            tree1

          case EmptyTree =>
            // no need to do anything, in particular, don't set the type to null, EmptyTree.tpe_= asserts
            tree

          case sup@Super(ths, name) if !name.isEmpty =>
            global.reporter.error(tree.pos, "Using named super is not supported in miniboxed classes. " +
                                            "For a workaround, please see " +
                                            "https://github.com/miniboxing/miniboxing-plugin/issues/166:")
            typed(gen.mkMethodCall(definitions.Predef_???, Nil), mode, pt).setType(ErrorType)

          case _ =>
            debuglog("Duplicators default case: " + tree.summaryString)
            debuglog(" ---> " + tree)

            if (tree.hasSymbolField && tree.symbol != NoSymbol && (tree.symbol.owner == definitions.AnyClass)) {
              tree.symbol = NoSymbol // maybe we can find a more specific member in a subclass of Any (see AnyVal members, like ==)
            }
            tree.setType(null)
            //println("dupl: " + ntree + ":" + tree.tpe + "  " + pt)
            val res = super.typed(tree, mode, pt)
            //println(res + " ==> " + res.tpe + "  (" + pt + ")")
            res
        }

        // TODO: The duplicator should propagate attachments
        // res.setAttachments(tree.attachments)

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
