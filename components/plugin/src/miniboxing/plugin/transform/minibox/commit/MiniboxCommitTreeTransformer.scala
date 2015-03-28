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
package transform
package minibox
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

    override def transform(tree: Tree): Tree = {
      var mbArray_transform = true

      if (flag_rewire_mbarray && !flag_two_way) {
        mbArray_transform = false
        global.reporter.echo("Miniboxing plugin warning: Optimizing `MbArray` is only possible if you allow " +
                             "the plugin to use both long and double encodings (remove `-P:minibox:Yone-way` " +
                             "compiler option). `MbArray`-s will be generic and will box.")
      } else if (!flag_rewire_mbarray) {
        mbArray_transform = false
        global.reporter.echo("Miniboxing plugin warning: Optimizing `MbArray` is disabled, thus `MbArray`-s will " +
                             "be generic and will box.")
      }

      val specTrans = new MiniboxTreeTransformer(unit, mbArray_transform)
      afterMiniboxCommit(checkNoStorage(specTrans.transform(tree)))
    }
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

  class MiniboxTreeTransformer(unit: CompilationUnit, mbArray_transform: Boolean) extends TypingTransformer(unit) {

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

          // MbArray transformations: MbArray.apply
          case tree@BoxToMinibox(Apply(fun @ Select(array, _), args), targ, repr) if fun.symbol == MbArray_apply && mbArray_transform =>
            val tags = minibox.typeTagTrees(currentOwner)
            val tag1 = tags(targ.dealiasWiden.typeSymbol)
            val tree1 = gen.mkMethodCall(MbArrayOpts_apply(repr), List(targ), transform(array) :: args.map(transform) ::: List(tag1))
            localTyper.typed(tree1)

          // MbArray transformations: MbArray.apply if primitive type
          case tree@Apply(fun @ Select(array, _), args) if fun.symbol == MbArray_apply && mbArray_transform && ScalaValueClasses.contains(tree.tpe.dealiasWiden.typeSymbol) =>
            val tpeSym = tree.tpe.dealiasWiden.typeSymbol
            val tags = minibox.typeTagTrees(currentOwner)
            val tag1 = tags(tpeSym)
            val repr: Symbol = PartialSpec.valueClassRepresentation(tpeSym)
            val tree1 = gen.mkMethodCall(MbArrayOpts_apply(repr), List(tree.tpe.dealiasWiden), transform(array) :: args.map(transform) ::: List(tag1))
            val tree2 = gen.mkMethodCall(minibox2x(repr)(tpeSym), List(tree1))
            localTyper.typed(tree2)

          // MbArray transformations: MbArray.update
          case Apply(fun @ Select(array, _), List(index, MiniboxToBox(value, targ, repr))) if fun.symbol == MbArray_update && mbArray_transform =>
            val tags = minibox.typeTagTrees(currentOwner)
            val tag1 = tags(targ.dealiasWiden.typeSymbol)
            val tree1 = gen.mkMethodCall(MbArrayOpts_update(repr), List(targ), transform(array) :: transform(index) :: transform(value) :: tag1 :: Nil)
            localTyper.typed(tree1)

          // MbArray transformations: MbArray.update if primitive type
          case Apply(fun @ Select(array, _), List(index, tree)) if fun.symbol == MbArray_update && mbArray_transform && ScalaValueClasses.contains(tree.tpe.dealiasWiden.typeSymbol) =>
            val tpeSym = tree.tpe.dealiasWiden.typeSymbol
            val tags = minibox.typeTagTrees(currentOwner)
            val tag1 = tags(tpeSym)
            val repr: Symbol = PartialSpec.valueClassRepresentation(tpeSym)
            val tree1 = gen.mkMethodCall(x2minibox(repr)(tpeSym), List(transform(tree)))
            val tree2 = gen.mkMethodCall(MbArrayOpts_update(repr), List(tree.tpe.dealiasWiden), transform(array) :: transform(index) :: tree1 :: tag1 :: Nil)
            localTyper.typed(tree2)

          // MbArray transformations: MbArray.empty and MbArray.clone
          case tree@Apply(TypeApply(fun, List(targ)), List(arg)) if (fun.symbol == MbArray_empty || fun.symbol == MbArray_clone) && mbArray_transform =>
            val pspec = PartialSpec.specializationsFromOwnerChain(currentOwner).toMap
            val tpe1 = targ.tpe
            val typeSymbol = tpe1.dealiasWiden.typeSymbol.deSkolemize

            def specialize(repr: Symbol): Tree = {
              val tags = minibox.typeTagTrees(currentOwner)
              val tag1 = tags(typeSymbol)
              val tree1 = gen.mkMethodCall(MbArrayOpts_alternatives(fun.symbol)(repr), List(tpe1), transform(arg) :: tag1 :: Nil)
              localTyper.typed(tree1)
            }

            pspec.get(typeSymbol) match {
              case Some(minibox.Miniboxed(repr))                  => specialize(repr)
              case Some(minibox.Boxed)                            => super.transform(tree)
              case None if tpe1 <:< AnyRefTpe                     => super.transform(tree)
              case None if ScalaValueClasses.contains(typeSymbol) => specialize(PartialSpec.valueClassRepresentation(typeSymbol))
              case _ =>
                var pos = tree.pos
                if (pos == NoPosition) pos = fun.pos
                if (pos == NoPosition) pos = arg.pos
                suboptimalCodeWarning(pos, "The following code instantiating an `MbArray` object cannot be optimized since the type argument is not a primitive type (like Int), a miniboxed type parameter or a subtype of AnyRef. This means that primitive types could end up boxed:", typeSymbol.isGenericAnnotated)
                super.transform(tree)
            }

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
            val pspec = PartialSpec.fromTargsAllTargs(NoPosition, conv.symbol.typeParams zip targs, currentOwner)
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
                           |Diagnostics:
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
                         |Diagnostics:
                         |  tree:  ${tree0}
                         |  pspec: $pspec
                         |  tags:  ${conv.symbol}
                      """.stripMargin)
                  localTyper.typed(gen.mkMethodCall(Predef_???, Nil))
              }
            } else
              super.transform(tree0)

          // Reflection-based constant folding
          // Warning: this may not be correct!
          case If(Literal(Constant(cond: Boolean)), thenb, elseb) =>
            if (cond) thenb else elseb

          case _ =>
            super.transform(tree0)
        }

      tree1.setType(newTpe)
    }
  }

  class OptimizeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    import global._
    import minibox._

//    override

  }

}
