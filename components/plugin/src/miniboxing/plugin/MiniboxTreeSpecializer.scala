package miniboxing.plugin

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.TypingTransformers
import scala.collection.mutable
import scala.tools.nsc.typechecker._

trait MiniboxTreeSpecializer extends TypingTransformers {
  self: MiniboxComponent =>

  import global._
  import definitions._
  import Flags._
  import typer.{ typed, atOwner }

  /** A tree transformer that prepares a tree for duplication */
  class MiniboxTreePreparer(unit: CompilationUnit,
                             miniboxedSyms: List[(Symbol, Type)],
                             miniboxedDeepEnv: Map[Symbol, Type],
                             miniboxedTags: Map[Symbol, Tree],
                             miniboxedReturn: Boolean) extends TypingTransformer(unit) {
    import global._
    override def transform(tree: Tree): Tree = tree match {
      case ddef@DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        localTyper.typed(deriveDefDef(ddef)(rhs => miniboxReturn(boxArgs(rhs))))
      case vdef@ValDef(mods, name, tpt, rhs) =>
        localTyper.typed(deriveValDef(vdef)(rhs => miniboxReturn(boxArgs(rhs))))
      case _ =>
        sys.error("incorrect use of miniboxed tree preparer")
    }

    val miniboxedArgs = miniboxedSyms.map(_._1)
    val miniboxedDeepEnvInv = miniboxedDeepEnv.map({ case (p, t) => (t.typeSymbol, p)})

    /** Wrap miniboxed arguments in minibox2box already */
    object boxArgs extends Transformer {
      def apply(tree: Tree) = transform(tree)
      override def transform(tree: Tree) = tree match {
        case i: Ident if miniboxedArgs.contains(i.symbol) =>
          val sym = i.symbol
          val idx = miniboxedArgs.indexOf(sym)
          val tsp = miniboxedSyms.find(_._1 == sym).get._2
          val tp  = miniboxedDeepEnvInv(tsp.typeSymbol)
          val tag = miniboxedTags(tsp.typeSymbol)
          localTyper.typed(gen.mkMethodCall(minibox2box, List(tp.tpe), List(gen.mkAttributedIdent(sym), tag)))
        case _ =>
          super.transform(tree)
      }
    }

    /** Wrap the return type in a box2minibox if necessary */
    def miniboxReturn(tree: Tree): Tree =
      if (miniboxedReturn) {
        tree match {
          case EmptyTree =>
            EmptyTree
          case Block(stats, res) =>
            // propagate inside blocks to enable the peephole optimization
            localTyper.typed(Block(stats, miniboxReturn(res)))
          case _ =>
            val tp = tree.tpe
            val updatedTpe = miniboxedDeepEnv.getOrElse(tp.typeSymbol, tp) // Nothing/Null stay the same
            val typeTag = miniboxedTags(updatedTpe.typeSymbol)
            localTyper.typed(gen.mkMethodCall(box2minibox, List(tp), List(tree, typeTag)))
        }
      } else
        tree
  }

  /** A tree transformer that transforms Tsp-s to Longs */
  class MiniboxTreeSpecializer(unit: CompilationUnit,
                                miniboxedSymsInit: List[(Symbol, Type)],
                                miniboxedTags: Map[Symbol, Tree],
                                miniboxedShallowEnv: Map[Symbol, Type]) extends TypingTransformer(unit) {


    // copied over from duplicators
    val shallowEnv = miniboxedShallowEnv.toList
    object miniboxedEnv extends SubstTypeMap(shallowEnv.map(_._1), shallowEnv.map(_._2)) {
      protected override def matches(sym1: Symbol, sym2: Symbol) =
        if (sym2.isTypeSkolem) sym2.deSkolemize eq sym1
        else sym1 eq sym2
    }

    var miniboxedSyms: List[(Symbol, Type)] = miniboxedSymsInit

    var indent = 0
    var treen = 0

    import global._
    import definitions._
    override def transform(tree: Tree): Tree = tree

    def boxingTransform(tree: Tree): Tree = {
      // printing vars:
      indent += 1
      treen += 1
      debug("  " * indent + "     (" + treen + ") tree: " + tree.toString.replaceAll("\n", "\n" + "  " * indent))

      val res = tree match {
//        case vdef @ ValDef(mods, name, tpt, rhs) =>
//          val tp = tree.symbol.tpe
//          val tpm = miniboxedEnv(tp)
//          if ((tpm =:= LongTpe) && !(tp =:= LongTpe)) {
//            // TODO: Do this
//            val rhs2 = gen.mkMethodCall(box2minibox, List(tp), List(rhs, miniboxedTags(tp.typeSymbol)))
//            val tpt2 = tpt.setType(LongClass.tpe)
//            var nvdef: Tree = copyValDef(vdef)(mods, name, tpt2, rhs2)
//            nvdef.symbol = vdef.symbol.modifyInfo(miniboxedEnv)
//            nvdef.tpe = null
//            nvdef = localTyper.typed(nvdef)
//            miniboxedSyms ::= (nvdef.symbol, tp)
//            nvdef
//          } else {
//            deriveValDef(vdef)(boxingTransform)
//          }
//        case id: Ident =>
//          ???
        case other => super.transform(other)
      }
      debug("  " * indent + "     (" + treen + ") res:  " + res.toString.replaceAll("\n", "\n" + "  " * indent))
      indent -= 1
      res
    }
  }
}
