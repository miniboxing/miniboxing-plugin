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
    override def transform(tree: Tree): Tree = {
      val res = afterInteropCommit(checkNoMbFunction(specTrans.transform(tree)))
      // clear the two functions
      FunctionsObjectSymbol.info.decls unlink marker_fun2mbfun
      FunctionsObjectSymbol.info.decls unlink marker_mbfun2fun
      // in newer versions of Scalac, the type history is refreshed for older
      // symbols, thus we need to reset flags and refresh the type history by hand
      minibox.refreshTypeHistory()
      res
    }
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

  object MaybeTyped {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Typed(tree, tpe) => Some(tree)
      case other: Tree      => Some(other)
    }
  }

  class InteropTreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree0: Tree): Tree = {
      val oldTpe = tree0.tpe
      val newTpe = deepTransformation(oldTpe)
//      println(oldTpe + " ==> " + newTpe)

      // force new info on the symbol
      if (tree0.hasSymbolField)
        tree0.symbol.info

      val tree1 =
        tree0 match {

          // update the type for transformed anonymous functions
          case cdef@ClassDef(mods, name, tparams, tpl@Template(parents, self, body)) if { cdef.symbol.info; transformedAnonFunctions(cdef.symbol) } =>
            val parents2 = cdef.symbol.info.parents.map(TypeTree(_))
            val impl2 = treeCopy.Template(tpl, parents2, self, transformStats(body, cdef.symbol))
            val cdef2 = treeCopy.ClassDef(cdef, mods, name, tparams, impl2)
            localTyper.typed(cdef2)

          // update the constructor for transformed anonymous functions, when represented as MiniboxedFunctionX
          case FunToMbFun(Typed(ctor @ Apply(Select(New(tpt), nme.CONSTRUCTOR), Nil), _), targ) if { tpt.symbol.info; transformedAnonFunctions(tpt.symbol) } =>
            val ctor2 = Apply(Select(New(tpt), nme.CONSTRUCTOR), Nil)
            localTyper.typed(ctor2)

          // update the constructor for transformed anonymous functions, when represented as FunctionX
          case Typed(ctor @ Apply(Select(New(tpt), nme.CONSTRUCTOR), Nil), _) if { tpt.symbol.info; transformedAnonFunctions(tpt.symbol) } =>
            val ctor2 = Apply(Select(New(tpt), nme.CONSTRUCTOR), Nil)
            val conv = gen.mkMethodCall(Select(ctor2, libraryFunctionName), Nil)
            localTyper.typed(conv)

          // update the super constructor
          case MaybeTyped(Apply(Select(Super(qual, _), nme.CONSTRUCTOR), Nil)) if transformedAnonFunctions(qual.symbol) =>
            val sup2 = Apply(Select(Super(This(qual.symbol), tpnme.EMPTY), nme.CONSTRUCTOR), Nil)
            val sup3 = localTyper.typed(sup2)
            sup3

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
            val tree1 = gen.mkMethodCall(Select(transform(tree), libraryFunctionName), Nil)
            localTyper.typed(tree1)

          case Select(MbFunToFun(fun, targ), _) if directMethodSymbols.contains(tree0.symbol) =>
            val tree1 = gen.mkAttributedSelect(transform(fun), directMethodUpdate(tree0.symbol))
            val res = localTyper.typedOperator(tree1)
            res

          case Select(fun, _) if flag_rewire_functionX_application &&
                                 directMethodSymbols.contains(tree0.symbol) =>

            // find the type argument
            val (conversion, targs) =
              tree0.symbol.owner match {
                case Function0Class => (function0_bridge, fun.tpe.baseType(Function0Class).typeArgs)
                case Function1Class => (function1_bridge, fun.tpe.baseType(Function1Class).typeArgs)
                case Function2Class => (function2_bridge, fun.tpe.baseType(Function2Class).typeArgs)
              }

            val tree1 = gen.mkMethodCall(conversion, targs, List(transform(fun)))
            val tree2 = gen.mkAttributedSelect(tree1, directMethodUpdate(tree0.symbol))
            val res = localTyper.typedOperator(tree2)
             res

          case _ =>
            super.transform(tree0)
        }

      tree1.setType(newTpe)
    }
  }
}