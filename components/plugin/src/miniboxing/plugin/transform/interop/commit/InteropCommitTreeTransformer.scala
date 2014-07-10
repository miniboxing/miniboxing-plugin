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
package miniboxing
package plugin
package transform
package interop
package commit

import scala.tools.nsc.transform.InfoTransform
import scala.tools.nsc.transform.TypingTransformers

trait InteropCommitTreeTransformer extends TypingTransformers {
  self: InteropCommitComponent =>

  import global._
  import definitions._
  import interop._

  def newTransformer(unit: CompilationUnit) = new Transformer {
    val specTrans = new InteropTreeTransformer(unit)
    override def transform(tree: Tree): Tree =
      afterInteropCommit(checkNoMbFunction(specTrans.transform(tree)))
  }

  class InteropTreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree) = tree
  }

  def checkNoMbFunction(tree: Tree) = {
    for (t <- tree)
      assert(noMbFunctionAnnot(t.tpe), t + ": " + t.tpe)
    tree
  }

  def noMbFunctionAnnot(t: Type): Boolean = {
    var hasStorage = false
    new TypeMap {
      def apply(tp: Type): Type = mapOver(tp)
      override def mapOver(tp: Type): Type = tp match {
        case _ if tp hasAnnotation(mbFunctionClass) =>
          hasStorage = true
          tp
        case _ =>
          super.mapOver(tp)
      }
    }.apply(t)

//    !hasStorage
    true
  }

  class CoercionExtractor {
    def unapply(tree: Tree, sym: Symbol): Option[(Tree, Type)] = tree match {
      case Apply(TypeApply(fun, List(targ)), List(inner)) if fun.symbol == sym => Some((inner, targ.tpe))
      case _ => None
    }
  }

  object MbFunToFun extends CoercionExtractor {
    def unapply(tree: Tree): Option[(Tree, Type)] = unapply(tree, marker_mbfun2fun).map({ case (tree, tpe) => (tree, tpe) })
  }

  object FunToMbFun extends CoercionExtractor {
    def unapply(tree: Tree): Option[(Tree, Type)] = unapply(tree, marker_fun2mbfun).map({ case (tree, tpe) => (tree, tpe) })
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

          case FunToMbFun(tree, targ) =>

            val (conversion, targs) =
              targ match {
                case TypeRef(_, Function0Class, targs) => (function0_bridge, targs)
                case TypeRef(_, Function1Class, targs) => (function1_bridge, targs)
                case TypeRef(_, Function2Class, targs) => (function2_bridge, targs)
              }

            val tree1 = gen.mkMethodCall(conversion, targs, List(transform(tree)))
            localTyper.typed(tree1)

          case MbFunToFun(tree, targ) =>
            val tree1 = gen.mkMethodCall(Select(transform(tree), newTermName("f")), Nil)
            localTyper.typed(tree1)

//          case MiniboxToMinibox(tree, targ, repr1, repr2) =>
//            val tree1 = gen.mkMethodCall(unreachableConversion, List(Literal(Constant(repr1.nameString)), Literal(Constant(repr2.nameString))))
//            localTyper.typed(tree1)

          case _ =>
            super.transform(tree0)
        }

      tree1.setType(newTpe)
    }
  }
}