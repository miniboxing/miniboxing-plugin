package miniboxing.plugin

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.TypingTransformers
import scala.collection.mutable.Set
import scala.tools.nsc.typechecker._
import scala.collection.mutable.{ Map => MMap }

trait MiniboxPostTreeTransformer extends TypingTransformers {
  self: MiniboxSpecComponent =>

  import global._
  import definitions._
  import minibox._
  import Flags._
  import typer.{ typed, atOwner }

  override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    val specTrans = new MiniboxTreeTransformer(unit)
    override def transform(tree: Tree): Tree =
      afterMiniboxSpec(checkNoStorage(specTrans.transform(tree)))
  }

  def checkNoStorage(tree: Tree) = {
    println(tree)
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

  class MiniboxTreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree: Tree): Tree = specTransform(tree, constraint = NoConstraint)

    var indent = 0

    def specTransform(tree0: Tree, constraint: Constraint): Tree = {
//      val crtindent = indent
//      indent += 2
//      println(" " * crtindent + " --> " + tree0)

      val origTpe = tree0.tpe

      // force new info on the symbol
      if (tree0.hasSymbol)
        tree0.symbol.info

      // constraints-based tree transformations
      val tree1 = tree0 match {
        case Apply(fun, args) =>
          val funTpe = fun.tpe
          val funParamTypes = beforeMiniboxSpec(funTpe.paramTypes)
          val nfun = transform(fun)
          // adapted arguments
          // println()
          // println("start")
          // println(args.map(t => t + "  " + t.tpe))
          val nargs =
            for ((arg, ptpe) <- args zip funParamTypes)
              yield {
                // println("arg: " + arg + "  boxed = " + !ptpe.hasAnnotation(StorageClass))
                specTransform(arg, constraint = if (ptpe.hasAnnotation(StorageClass)) Miniboxed else Boxed)
              }
          val ntree = Apply(nfun, nargs)
          // println(ntree)
          // println(args zip funParamTypes zip nargs)
          // println(nfun.tpe)
          // println(nargs.map(t => t + "  " + t.tpe))
          // println("stop")
          localTyper.typed(ntree)
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) if rhs != EmptyTree =>
          val origRet = beforeMiniboxSpec(tree0.symbol.tpe.finalResultType)
          val constr = if (origRet.hasAnnotation(StorageClass)) Miniboxed else Boxed
          val ntparams = tparams.map(transform(_).asInstanceOf[TypeDef])
          val nvparamss = vparamss.map(_.map(transform(_).asInstanceOf[ValDef]))
          val ntpt = transform(tpt)
          val nrhs = atOwner(tree0.symbol)(localTyper.typed(specTransform(rhs, constr)))
          val ntree = DefDef(mods, name, ntparams, nvparamss, ntpt, nrhs).setSymbol(tree0.symbol)
          //  println(tree0.symbol)
          //  println(origRet)
          //  println(ntree)
          localTyper.typed(ntree)
        case ValDef(mods, name, tpt, rhs) if rhs != EmptyTree =>
          val origRet = beforeMiniboxSpec(tree0.symbol.tpe)
          val constr = if (origRet.hasAnnotation(StorageClass)) Miniboxed else Boxed
          val ntpt = transform(tpt)
          val nrhs = specTransform(rhs, constr)
          val ntree = ValDef(mods, name, ntpt, nrhs).setSymbol(tree0.symbol)
          localTyper.typed(ntree)
        // TODO: LabelDef
        case _ =>
          super.transform(tree0)
      }

      // conversion-based tree transformations
      val tree2 = tree1 match {
        case EmptyTree =>
          EmptyTree
        case _ =>
          val oldTpe = origTpe
          val newTpe = deepTransformation(oldTpe)
          tree1.tpe = newTpe

          val ntree =
            constraint match {
              case NoConstraint =>
                //  TODO: optimistically minibox values
                //  lazy val tags = typeTagTrees(currentMethod, currentClass)
                //  if (!tree1.isInstanceOf[TypeTree] &&
                //       tree1.tpe.typeSymbol.isTypeParameterOrSkolem &&
                //       tags.isDefinedAt(tree1.tpe.typeSymbol))
                //    localTyper.typed(convert_box_to_minibox(tree1, currentMethod, currentClass))
                //  else
                //    tree1
                tree1
              case Boxed =>
                if (oldTpe.hasAnnotation(StorageClass))
                  localTyper.typed(convert_minibox_to_box(tree1, oldTpe.withoutAnnotations, currentMethod, currentClass))
                else
                  tree1
              case Miniboxed =>
                if (oldTpe.hasAnnotation(StorageClass))
                  tree1
                else
                  localTyper.typed(convert_box_to_minibox(tree1, currentMethod, currentClass))
            }

          assert(noStorageAnnot(ntree.tpe))
          ntree
      }

//      indent -= 2
//      println(" " * crtindent + " --> " + tree2)

      tree2
    }
  }
}
