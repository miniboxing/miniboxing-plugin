package plugin


import scala.reflect.internal.Flags
import scala.tools.nsc.transform.TypingTransformers
import scala.collection.mutable.ListBuffer

trait MiniboxTreeTransformation extends TypingTransformers {
  self: MiniboxLogic with MiniboxLogging =>

  import global._
  import definitions._
  import Flags._
  import typer.{ typed, atOwner }

  private lazy val TagDipatchObjectSymbol = definitions.getRequiredModule("runtime.MiniboxTypeTagDispatch")
  private lazy val array_update = definitions.getMember(TagDipatchObjectSymbol, newTermName("array_update"))
  private lazy val array_apply = definitions.getMember(TagDipatchObjectSymbol, newTermName("array_apply"))
  private lazy val tag_hashCode = definitions.getMember(TagDipatchObjectSymbol, newTermName("hashCode"))
  private lazy val tag_## = definitions.getMember(TagDipatchObjectSymbol, newTermName("hashhash"))
  
  private lazy val ConversionsObjectSymbol = definitions.getRequiredModule("runtime.MiniboxConversions")
  private lazy val minibox2box = definitions.getMember(ConversionsObjectSymbol, newTermName("minibox2box"))

  class MiniboxTreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree: Tree): Tree = {
      curTree = tree
      tree match {
        case Apply(fn, args) =>
          val methodSym = tree.symbol

          // box the arguments if necessary
          val newArgs =
            (methodSym.tpe.paramTypes zip (transformTrees(args) map (a => (a, a.tpe)))) map {
              case (paramType, (arg, argType)) =>
                if (paramType == AnyClass.tpe && isMiniboxed(argType.typeSymbol))
                  box(arg)
                else
                  arg
            }

          fn match {
            case sel @ Select(qual, meth) =>
              var newTree: Tree = null

              /*
               * Dispatch on a variable of type T:
               *  - for hashCode and ##, use an optimized function
               *  - for the rest, box
               */
              if (isMiniboxed(qual)) {
                if (methodSym == Any_##) {
                    newTree = gen.mkMethodCall(tag_##, transform(qual) :: newArgs)
                } else if (methodSym == Any_hashCode) {
                    newTree = gen.mkMethodCall(tag_hashCode, transform(qual) :: newArgs)
                } else {
                  newTree = gen.mkMethodCall(box(transform(qual)), methodSym, Nil, newArgs)
                }
              }
//              println(runtime.this.MiniboxConversions.MiniboxToBoolea(0))
              /*
               * For array access instructions, use the static methods from MiniboxTypeTagDispatch  
               */
              if (methodSym == Array_apply && isMiniboxed(tree)) {
                newTree = gen.mkMethodCall(array_apply, transform(qual) :: newArgs)
              }
              if (methodSym == Array_update && isMiniboxed(args(1))) {
                newTree = gen.mkMethodCall(array_update, transform(qual) :: newArgs)
              }

              if (newTree == null)
                newTree = treeCopy.Apply(tree, transform(fn), newArgs)
              localTyper.typed(atPos(tree.pos)(newTree))
            case _ =>
              localTyper.typed(atPos(tree.pos)(treeCopy.Apply(tree, transform(fn), newArgs)))
          }

        /*
         * Assignment, valdef, return => box
         * HACK: There are also other cases where boxing has to be done by hand :D
         */
        case ddef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) if (tpt.tpe == AnyClass.tpe && isMiniboxed(rhs)) =>
          val newrhs = box(transform(rhs))
          localTyper.typed(atPos(tree.pos)(deriveDefDef(ddef)(_ => newrhs)))

        case vdef @ ValDef(mods, name, tpt, rhs) if (tpt.tpe == AnyClass.tpe && isMiniboxed(rhs)) =>
          val newrhs = box(transform(rhs))
          localTyper.typed(atPos(tree.pos)(deriveValDef(vdef)(_ => newrhs)))

          
        // Insert a cast if necessary
        case Assign(lhs, rhs) if (lhs.tpe == AnyClass.tpe && isMiniboxed(rhs)) =>
          localTyper.typed(atPos(tree.pos)(Assign(lhs, box(transform(rhs)))))

          
        // Create empty bodies for methods that have only symbols
        case Template(parents, self, body) =>
          val specMembers = createMethodTrees(tree.symbol.enclClass) map localTyper.typed
          localTyper.typedPos(tree.pos)(
            treeCopy.Template(tree, parents, self, atOwner(currentOwner)(transformTrees(body ::: specMembers))))

        case _ =>
          //println("descending into: " + tree.printingPrefix + " " +  tree)
          super.transform(tree)
      }
    }

    /*
     * Returns the code to box around some tree 
     */
    private def box(tree: Tree) = localTyper.typed(gen.mkMethodCall(minibox2box, List(tree)))

    /*
     * Tells whether a tree computes a value that will be stored on Long and
     * will have a Manifest[T].
     * 
     * XXX: now it's a hack
     * The following would work for sure if we add the annotation:
     *   t.tpe.typeSymbol hasAnnotation MinispecedClass 
     */
    private def isMiniboxed(t: Tree) = t.tpe.typeSymbol isTypeParameter
    private def isMiniboxed(typ: Symbol) = typ.isTypeParameter

    /**
     * In `MiniboxInfoTransform` we create only symbols for methods.
     * Here we add empty bodies for them.
     */
    private def createMethodTrees(sClass: Symbol): List[Tree] = {
      val mbrs = new ListBuffer[Tree]
      for (m <- sClass.info.decls if m.hasFlag(SPECIALIZED)) {
        debuglog("creating tree for " + m.fullName)
        if (m.isMethod) {
          mbrs += atPos(m.pos)(DefDef(m, { paramss => EmptyTree }))
        } else if (m.isValue) {
          mbrs += ValDef(m, EmptyTree).setType(NoType).setPos(m.pos)
        }
      }
      mbrs.toList
    }
  }
}
