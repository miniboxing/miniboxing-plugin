package miniboxing.plugin
package transform
package spec

import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.typechecker._

trait MiniboxPostTreeTransformer extends TypingTransformers {
  self: MiniboxSpecComponent =>

  import global._
  import definitions._
  import minibox._
  import typer.{ typed, atOwner }

  override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    val specTrans = new MiniboxTreeTransformer(unit)
    override def transform(tree: Tree): Tree =
      afterMiniboxSpec(checkNoStorage(specTrans.transform(tree)))
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
    def unapply(tree: Tree, sym: Symbol): Option[(Tree, Type)] = tree match {
      case Apply(TypeApply(fun, List(targ)), List(inner)) if fun.symbol == sym => Some(inner, targ.tpe)
      case _ => None
    }
  }

  object MiniboxToBox extends CoercionExtractor {
    def unapply(tree: Tree): Option[(Tree, Type)] = unapply(tree, marker_minibox2box)
  }

  object BoxToMinibox extends CoercionExtractor {
    def unapply(tree: Tree): Option[(Tree, Type)] = unapply(tree, marker_box2minibox)
  }

  class MiniboxTreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree0: Tree): Tree = {
      val oldTpe = tree0.tpe
      val newTpe = deepTransformation(oldTpe)
//      println(oldTpe + " ==> " + newTpe)

      // force new info on the symbol
      if (tree0.hasSymbol)
        tree0.symbol.info

      val tree1 =
        tree0 match {

          // Array application
          case BoxToMinibox(tree@Apply(apply @ Select(array, _), List(pos)), _) if apply.symbol == Array_apply =>
            val tags = typeTagTrees(currentOwner)
            val tree1 = array.tpe.widen.typeArgs match {
              case tpe :: Nil if tags.isDefinedAt(tpe.typeSymbol) =>
                val tag = tags(tpe.typeSymbol)
                val tree1 = gen.mkMethodCall(mbarray_apply, List(transform(array), transform(pos), tag))
                stats("rewrote array apply: " + tree + " ==> " + tree1)
                tree1
              case _ =>
                super.transform(tree)
            }
            localTyper.typed(tree1)

          // Array update
          case tree@Apply(update@Select(array, _), List(pos, MiniboxToBox(element, _))) if update.symbol == Array_update =>
            val tags = typeTagTrees(currentOwner)
            val tree1 = array.tpe.widen.typeArgs match {
              case tpe :: Nil if tags.isDefinedAt(tpe.typeSymbol) =>
                val tag = tags(tpe.typeSymbol)
                val tree2 = gen.mkMethodCall(mbarray_update, List(transform(array), transform(pos), transform(element), tag))
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
                stats("rewrote array new: " + tree + " ==> " + tree1)
                tree1
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
          case tree@Apply(Select(MiniboxToBox(val1, targ1), eqeq), List(MiniboxToBox(val2, targ2))) if tree.symbol == Any_== =>
            val tags = typeTagTrees(currentOwner)
            val tag1 = tags(targ1.dealiasWiden.typeSymbol)
            val tag2 = tags(targ2.dealiasWiden.typeSymbol)
            val tree1 = {
              if ((tag1 == tag2) || (tag1.symbol == tag2.symbol))
                gen.mkMethodCall(notag_==, List(transform(val1), transform(val2)))
              else
                gen.mkMethodCall(tag_==, List(transform(val1), tag1, transform(val2), tag2))
            }
            localTyper.typed(tree1)

          // simplify equality between miniboxed values 2 - comparison with other values
          case tree@Apply(Select(MiniboxToBox(val1, targ1), eqeq), List(arg)) if tree.symbol == Any_== =>
            val tags = typeTagTrees(currentOwner)
            val tag1 = tags(targ1.dealiasWiden.typeSymbol)
            val tree1 = gen.mkMethodCall(other_==, List(transform(val1), tag1, transform(arg)))
            localTyper.typed(tree1)

          // simplify hashCode
          case tree@Apply(Select(MiniboxToBox(val1, targ1), hash), _) if tree.symbol == Any_hashCode =>
            val tags = typeTagTrees(currentOwner)
            val tag1 = tags(targ1.dealiasWiden.typeSymbol)
            val tree1 = gen.mkMethodCall(tag_hashCode, List(transform(val1), tag1))
            localTyper.typed(tree1)

          // simplify toString
          case tree@Apply(Select(MiniboxToBox(val1, targ1), toString), _) if tree.symbol == Any_toString =>
            val tags = typeTagTrees(currentOwner)
            val tag1 = tags(targ1.dealiasWiden.typeSymbol)
            val tree1 = gen.mkMethodCall(tag_toString, List(transform(val1), tag1))
            localTyper.typed(tree1)

          case BoxToMinibox(tree, targ) =>
            val tags = minibox.typeTagTrees(currentOwner)
            localTyper.typed(gen.mkMethodCall(box2minibox, List(targ), List(transform(tree), tags(targ.typeSymbol))))

          case MiniboxToBox(tree, targ) =>
            val tags = minibox.typeTagTrees(currentOwner)
            localTyper.typed(gen.mkMethodCall(minibox2box, List(targ), List(transform(tree), tags(targ.typeSymbol))))

          case _ =>
            super.transform(tree0)
        }

      tree1.setType(newTpe)
    }
  }
}
