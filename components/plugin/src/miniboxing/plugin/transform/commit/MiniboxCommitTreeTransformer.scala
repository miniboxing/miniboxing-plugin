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
package transform
package commit

import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.typechecker._

trait MiniboxCommitTreeTransformer extends TypingTransformers {
  self: MiniboxCommitComponent =>

  import global._
  import definitions._
  import minibox._
  import typer.{ typed, atOwner }

  override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    val specTrans = new MiniboxTreeTransformer(unit)
    override def transform(tree: Tree): Tree =
      afterMiniboxCommit(checkNoStorage(specTrans.transform(tree)))
  }

  def checkNoStorage(tree: Tree) = {
    //println(tree)
    for (t <- tree)
      assert(noStorageAnnot(t.tpe), t + ": " + t.tpe)
    tree
  }

  def noStorageAnnot(t: Type): Boolean = {
    var hasStorage = false
    new TypeMap {
      def apply(tp: Type): Type = mapOver(tp)
      override def mapOver(tp: Type): Type = tp match {
        case _ if tp hasAnnotation(StorageClass) =>
          hasStorage = true
          tp
        case _ =>
          super.mapOver(tp)
      }
    }.apply(t)

    !hasStorage
  }

  abstract sealed class Constraint
  case object Miniboxed extends Constraint
  case object Boxed extends Constraint
  case object NoConstraint extends Constraint

  class CoercionExtractor {
    def unapply(tree: Tree, sym: Symbol): Option[(Tree, Type, List[Symbol])] = tree match {
      // TODO: Return the storage too
      case Apply(TypeApply(fun, targ :: reprTpes), List(inner)) if fun.symbol == sym => Some((inner, targ.tpe, reprTpes.map(_.tpe.typeSymbol)))
      case _ => None
    }
  }

  object MiniboxToBox extends CoercionExtractor {
    def unapply(tree: Tree): Option[(Tree, Type, Symbol)] = unapply(tree, marker_minibox2box).map({ case (tree, tpe, List(repr)) => (tree, tpe, repr) })
  }

  object BoxToMinibox extends CoercionExtractor {
    def unapply(tree: Tree): Option[(Tree, Type, Symbol)] = unapply(tree, marker_box2minibox).map({ case (tree, tpe, List(repr)) => (tree, tpe, repr) })
  }

  object MiniboxToMinibox extends CoercionExtractor {
    def unapply(tree: Tree): Option[(Tree, Type, Symbol, Symbol)] = unapply(tree, marker_minibox2minibox).map({ case (tree, tpe, List(repr1, repr2)) => (tree, tpe, repr1, repr2) })
  }

  class MiniboxTreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree0: Tree): Tree = {
      val oldTpe = tree0.tpe
      val newTpe = deepTransformation(oldTpe)
//      println(oldTpe + " ==> " + newTpe)

      // force new info on the symbol
      if (tree0.hasSymbolField)
        tree0.symbol.info

      val tree1 =
        tree0 match {

          // Array application
          case BoxToMinibox(tree@Apply(apply @ Select(array, _), List(pos)), _, repr) if apply.symbol == Array_apply =>
            val tags = typeTagTrees(currentOwner)
            val tree1 = array.tpe.widen.typeArgs match {
              case tpe :: Nil if tags.isDefinedAt(tpe.typeSymbol) =>
                val tag = tags(tpe.typeSymbol)
                val tree1 = gen.mkMethodCall(mbarray_apply(repr), List(transform(array), transform(pos), tag))
                stats("rewrote array apply: " + tree + " ==> " + tree1)
                tree1
              case _ =>
                super.transform(tree)
            }
            localTyper.typed(tree1)

          // Array update
          case tree@Apply(update@Select(array, _), List(pos, MiniboxToBox(element, _, repr))) if update.symbol == Array_update =>
            val tags = typeTagTrees(currentOwner)
            val tree1 = array.tpe.widen.typeArgs match {
              case tpe :: Nil if tags.isDefinedAt(tpe.typeSymbol) =>
                val tag = tags(tpe.typeSymbol)
                val tree2 = gen.mkMethodCall(mbarray_update(repr), List(transform(array), transform(pos), transform(element), tag))
                stats("rewrote array update: " + tree + " ==> " + tree2)
                tree2
              case _ =>
                super.transform(tree)
            }
            localTyper.typed(tree1)

           // Array new
          case tree@Apply(newArray @ Select(manifest, _), List(size)) if newArray.symbol == Manifest_newArray =>
            val tags = typeTagTrees(currentOwner)
            val tree1 = manifest.tpe.widen.typeArgs match {
              case tpe :: Nil if tags.isDefinedAt(tpe.typeSymbol) =>
                val tag = tags(tpe.typeSymbol)
                val tree1 = gen.mkMethodCall(mbarray_new, List(tpe), List(transform(size), tag))
                val ptSym = tree.tpe.dealiasWiden.typeSymbol
                val tree2 =
                  if (ptSym == ArrayClass)
                    gen.mkAttributedCast(tree1, tree.tpe)
                  else
                    tree1
                stats("rewrote array new: " + tree + " ==> " + tree1)
                tree2
              case _ =>
                super.transform(tree)
            }
            localTyper.typed(tree1)

          // Array length
          case tree@Apply(length @ Select(array, _), Nil) if length.symbol == Array_length =>
            val tags = typeTagTrees(currentOwner)
            val tree1 = array.tpe.widen.typeArgs match {
              case tpe :: Nil if tags.isDefinedAt(tpe.typeSymbol) =>
                val tag = tags(tpe.typeSymbol)
                val tree1 = gen.mkMethodCall(mbarray_length, List(transform(array), tag))
                stats("rewrote array length: " + tree + " ==> " + tree1)
                tree1
              case _ =>
                super.transform(tree)
            }
            localTyper.typed(tree1)

          // simplify equality between miniboxed values
          case tree@Apply(Select(MiniboxToBox(val1, targ1, repr1), eqeq), List(MiniboxToBox(val2, targ2, repr2))) if (tree.symbol == Any_==) && (repr1 == repr2) =>
            val tags = typeTagTrees(currentOwner)
            val tag1 = tags(targ1.dealiasWiden.typeSymbol)
            val tag2 = tags(targ2.dealiasWiden.typeSymbol)
            val tree1 = {
              if ((tag1 == tag2) || (tag1.symbol == tag2.symbol))
                gen.mkMethodCall(notag_==(repr1), List(transform(val1), transform(val2)))
              else
                gen.mkMethodCall(tag_==(repr1), List(transform(val1), tag1, transform(val2), tag2))
            }
            localTyper.typed(tree1)

          // simplify equality between miniboxed values 2 - comparison with other values
          case tree@Apply(Select(MiniboxToBox(val1, targ1, repr), eqeq), List(arg)) if tree.symbol == Any_== =>
            val tags = typeTagTrees(currentOwner)
            val tag1 = tags(targ1.dealiasWiden.typeSymbol)
            val tree1 = gen.mkMethodCall(other_==(repr), List(transform(val1), tag1, transform(arg)))
            localTyper.typed(tree1)

          // simplify hashCode
          case tree@Apply(Select(MiniboxToBox(val1, targ1, repr), hash), _) if tree.symbol == Any_hashCode =>
            val tags = typeTagTrees(currentOwner)
            val tag1 = tags(targ1.dealiasWiden.typeSymbol)
            val tree1 = gen.mkMethodCall(tag_hashCode(repr), List(transform(val1), tag1))
            localTyper.typed(tree1)

          // simplify toString
          case tree@Apply(Select(MiniboxToBox(val1, targ1, repr), toString), _) if tree.symbol == Any_toString =>
            val tags = typeTagTrees(currentOwner)
            val tag1 = tags(targ1.dealiasWiden.typeSymbol)
            val tree1 = gen.mkMethodCall(tag_toString(repr), List(transform(val1), tag1))
            localTyper.typed(tree1)

          case BoxToMinibox(tree, targ, repr) =>
            val tags = minibox.typeTagTrees(currentOwner)
            val tree1 =
              x2minibox(repr).get(targ.typeSymbol) match {
                case Some(optConv) =>
                  gen.mkMethodCall(optConv, List(transform(tree))) // notice the fewer arguments
                case None =>
                  gen.mkMethodCall(box2minibox(repr), List(targ), List(transform(tree), tags(targ.typeSymbol)))
              }
            localTyper.typed(tree1)

          case MiniboxToBox(tree, targ, repr) =>
            val tags = minibox.typeTagTrees(currentOwner)
            val tree1 =
              minibox2x(repr).get(targ.typeSymbol) match {
                case Some(optConv) =>
                  gen.mkMethodCall(optConv, List(transform(tree))) // notice the fewer arguments
                case None =>
                  gen.mkMethodCall(minibox2box(repr), List(targ), List(transform(tree), tags(targ.typeSymbol)))
              }
            localTyper.typed(tree1)

          case MiniboxToMinibox(tree, targ, repr1, repr2) =>
            val tree1 = gen.mkMethodCall(unreachableConversion, List(Literal(Constant(repr1.nameString)), Literal(Constant(repr2.nameString))))
            localTyper.typed(tree1)

          case Apply(TypeApply(conv, _targs), _args) if interop.flag_rewire_functionX_bridges && interop.function_bridges(conv.symbol) =>
            val targs = _targs.map(transform(_).tpe)
            val args  = _args.map(transform)
            val bridge = conv.symbol
            val pspec = PartialSpec.fromTargsAllTargs(conv.symbol.typeParams, targs, currentOwner)
            val allTparsAreMboxed = !pspec.exists(_._2 == minibox.Boxed)
            if (allTparsAreMboxed) {
              val reprs = pspec.map({ case (tpar, minibox.Miniboxed(repr)) => repr; case _ => ??? }).toList

              interop.function_bridge_optimized.get(bridge).flatMap(_.get(reprs)) match {
                case Some(bridge_opt) =>
                  val tags = minibox.typeTagTrees(currentOwner)
                  val tag_targs = targs.map(_.typeSymbol)
                  val tag_opts = tag_targs.map(tags.get(_))
                  if (tag_opts.exists(_ == None)) {
                    global.reporter.error(tree0.pos,
                        s"""[miniboxing plugin internal error] Cannot find tag when rewriting function to miniboxed function bridge.
                           |Diagnostics:stripPrefix
                           |  tree:  ${tree0}
                           |  pspec: $pspec
                           |  tags:  $tags
                        """.stripMargin)
                  }
                  val tag_params = tag_opts.map(_.getOrElse(gen.mkMethodCall(Predef_???, Nil))).toList
                  val tree1 = gen.mkMethodCall(bridge_opt, targs, tag_params.map(localTyper.typed) ::: args)
                  localTyper.typed(tree1)
                case None =>
                  global.reporter.error(tree0.pos,
                      s"""[miniboxing plugin internal error] Cannot find tag when rewriting function to miniboxed function bridge.
                         |Diagnostics:stripPrefix
                         |  tree:  ${tree0}
                         |  pspec: $pspec
                         |  tags:  ${conv.symbol}
                      """.stripMargin)
                  localTyper.typed(gen.mkMethodCall(Predef_???, Nil))
              }
            } else
              super.transform(tree0)

          case _ =>
            super.transform(tree0)
        }

      tree1.setType(newTpe)
    }
  }
}
