package plugin

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.TypingTransformers
import scala.collection.mutable

trait MiniboxTreeTransformation extends TypingTransformers {
  self: MiniboxLogic with MiniboxLogging with MiniboxSpecializationInfo =>

  import global._
  import definitions._
  import Flags._
  import typer.{ typed, atOwner }
  import memberSpecializationInfo._

  private lazy val TagDipatchObjectSymbol =
    definitions.getRequiredModule("runtime.MiniboxTypeTagDispatch")
  private lazy val array_update =
    definitions.getMember(TagDipatchObjectSymbol, newTermName("array_update"))
  private lazy val array_apply =
    definitions.getMember(TagDipatchObjectSymbol, newTermName("array_apply"))
  private lazy val array_length =
    definitions.getMember(TagDipatchObjectSymbol, newTermName("array_length"))

  private lazy val tag_hashCode =
    definitions.getMember(TagDipatchObjectSymbol, newTermName("hashCode"))
  private lazy val tag_## =
    definitions.getMember(TagDipatchObjectSymbol, newTermName("hashhash"))

  private lazy val ConversionsObjectSymbol =
    definitions.getRequiredModule("runtime.MiniboxConversions")
  private lazy val minibox2box =
    definitions.getMember(ConversionsObjectSymbol, newTermName("minibox2box"))

  class MiniboxTreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree: Tree): Tree = {
      curTree = tree

      tree match {

        /*
         *  We have created just the symbols for the specialized classes - now
         *  it's time to create their trees as well (initially empty).
         *  
         */
        case PackageDef(pid, classdefs) =>
          tree.symbol.info // make sure specializations have been performed

          atOwner(tree, tree.symbol) {
            val specClasses = createSpecializedClassesTrees(classdefs) map localTyper.typed
            localTyper.typedPos(tree.pos)(
              treeCopy.PackageDef(
                tree,
                pid,
                transformStats(classdefs ::: specClasses, tree.symbol.moduleClass)))
          }

        /*
         * The tree of a specialized class is empty for the moment, but we
         * have creates symbols for the methods - give them an empty body.
         * 
         * Meanwhile, collect the bodies of the methods that need to be copied
         * and specialized.
         */
        case Template(parents, self, body) =>
          MethodBodiesCollector(tree)
          val specMembers = createMethodTrees(tree.symbol.enclClass) map localTyper.typed
          localTyper.typedPos(tree.pos)(
            treeCopy.Template(tree, parents, self,
              atOwner(currentOwner)(transformTrees(body ::: specMembers))))

        /*
         * A definition with empty body - add a body as prescribed by the
         * `methodSpecializationInfo` data structure. 
         */
        case ddef @ DefDef(mods, name, tparams, vparamss, tpt, EmptyTree) if hasInfo(ddef) =>
          memberSpecializationInfo(tree.symbol) match {
            case FieldAccessor(field) =>
              val rhs1 =
                if (tree.symbol.isGetter) {
                  localTyper.typed(
                    cast(gen.mkAttributedRef(field), tpt.tpe))
                } else {
                  localTyper.typed(
                    Assign(gen.mkAttributedRef(field),
                      cast(Ident(vparamss.head.head.symbol), field.tpe)))
                }
              localTyper.typed(deriveDefDef(tree)(_ => rhs1))

            case SpecializedImplementationOf(original) =>
              val rhs1 = MethodBodiesCollector.getMethodBody(original)._1
              localTyper.typed(deriveDefDef(tree)(_ => rhs1))

            case ForwardTo(target) =>
              val rhs1 = if (vparamss.isEmpty) {
                gen.mkAttributedRef(target)
              } else {
                val params1 =
                  (vparamss.head zip target.paramss.head) map {
                    case (p, t) => cast(Ident(p.symbol), t.tpe)
                  }
                gen.mkMethodCall(target, tparams map (_.tpe), params1)
              }

              localTyper.typed(deriveDefDef(tree)(_ => localTyper.typed(cast(rhs1, tpt.tpe))))

            case Interface() =>
              tree

            case OverrideOfSpecializedMethod(target) =>
              sys.error("Not yet implemented!")

            case info =>
              sys.error("Unknown info type: " + info)
          }

        case vdef @ ValDef(mods, name, tpt, EmptyTree) if hasInfo(vdef) =>
          memberSpecializationInfo(tree.symbol) match {
            case SpecializedImplementationOf(original) =>
              val rhs1 = MethodBodiesCollector.getMethodBody(original)._1
              localTyper.typed(deriveValDef(tree)(_ => rhs1))
            case info =>
              sys.error("Unknown info type: " + info)
          }

        case _ =>
          //          println("descending into: " + tree.printingPrefix + " " + tree)
          super.transform(tree)
      }
    }

    /**
     * In `MiniboxInfoTransform` we create only symbols for methods.
     * Here we add empty bodies for them.
     */
    private def createMethodTrees(sClass: Symbol): List[Tree] = {
      val mbrs = new mutable.ListBuffer[Tree]
      for (m <- sClass.info.decls if m hasFlag SPECIALIZED) {
        debug("creating tree for " + m.fullName)
        if (m.isMethod) {
          mbrs += atPos(m.pos)(DefDef(m, { paramss => EmptyTree }))
        } else if (m.isValue) {
          mbrs += ValDef(m, EmptyTree).setType(NoType).setPos(m.pos)
        }
      }
      mbrs.toList
    }

    /**
     * Create implementation trees for specialized classes
     */
    private def createSpecializedClassesTrees(classdefs: List[Tree]): List[Tree] = {
      val buf = new mutable.ListBuffer[Tree]
      for (tree <- classdefs)
        tree match {
          case ClassDef(_, _, _, impl) =>
            tree.symbol.info // force specialization
            val sym1 = tree.symbol
            val classSymbol = tree.symbol

            var sClasses: List[Symbol] = Nil

            if (specializedInterface isDefinedAt classSymbol) {
              sClasses ::= specializedInterface(classSymbol).sym
              sClasses ++= specializedClasses(classSymbol).values map (_.sym)
            }

            for (sClass <- sClasses) {
              debug("creating class - " + sClass.name + ": " + sClass.parentSymbols)
              val parents = sClass.info.parents map TypeTree
              buf +=
                ClassDef(sClass, atPos(impl.pos)(Template(parents, emptyValDef, List()))
                  .setSymbol(sClass.newLocalDummy(sym1.pos))) setPos tree.pos
            }

            println()
          case _ =>
        }
      buf.toList
    }

    /*
     * We collect the bodies of the target methods in order to have them available
     * for copying inside the methods that are specialized implementations of them.
     */
    private object MethodBodiesCollector extends Traverser {
      private val body = mutable.HashMap[Symbol, (Tree, List[Symbol])]()

      override def traverse(tree: Tree) = tree match {
        case DefDef(_, _, _, vparams :: Nil, _, rhs) if (templateMembers(tree.symbol)) =>
          collect(tree.symbol, rhs, vparams map (_.symbol))
        case DefDef(_, _, _, Nil, _, rhs) if (templateMembers(tree.symbol)) =>
          collect(tree.symbol, rhs, Nil)
        case ValDef(mods, name, tpt, rhs) if templateMembers(tree.symbol) =>
          collect(tree.symbol, rhs, Nil)
        case _ =>
          super.traverse(tree)
      }

      private def collect(member: Symbol, rhs: Tree, params: List[Symbol]) = {
        body(member) = (rhs, params)
        templateMembers -= member
        println("collected " + member.fullName)
      }

      def getMethodBody(meth: Symbol) = body(meth)
      def getFieldBody(fld: Symbol) = body(fld)._1
    }

    /**
     * Tree transformer that is used for inserting casts.
     *
     * NOTE: In the standard implementation of specialization such casts
     * are done during erasure, but in our case we need to look at the type
     * Manifest when boxing/unboxing, so a custom transformation needs to be
     * done.
     */
    private object TypeAdapter extends TypingTransformer(unit) {
      def adapt(tree: Tree): Tree = transform(tree)

      override def transform(tree: Tree): Tree = {
        curTree = tree
        tree match {
          // array_length with tag-based dispatch
          case Select(qual, meth) if isMiniboxedArray(qual) && tree.symbol == Array_length =>
            localTyper.typedPos(tree.pos)(
              gen.mkMethodCall(array_length, List(transform(qual))))

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
                /*
               * For array access instructions, use the static methods from 
               * MiniboxTypeTagDispatch  
               */
                if (methodSym == Array_apply && isMiniboxedArray(qual)) {
                  newTree = gen.mkMethodCall(array_apply, transform(qual) :: newArgs)
                }
                if (methodSym == Array_update && isMiniboxedArray(qual)) {
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
          case ddef @ DefDef(_, _, _, _, tpt, rhs) if (tpt.tpe == AnyClass.tpe && isMiniboxed(rhs)) =>
            val newrhs = box(transform(rhs))
            localTyper.typed(atPos(tree.pos)(deriveDefDef(ddef)(_ => newrhs)))

          case vdef @ ValDef(_, _, tpt, rhs) if (tpt.tpe == AnyClass.tpe && isMiniboxed(rhs)) =>
            val newrhs = box(transform(rhs))
            localTyper.typed(atPos(tree.pos)(deriveValDef(vdef)(_ => newrhs)))

          // Insert a cast if necessary
          case Assign(lhs, rhs) if (lhs.tpe == AnyClass.tpe && isMiniboxed(rhs)) =>
            localTyper.typed(atPos(tree.pos)(Assign(lhs, box(transform(rhs)))))

          case _ =>
            //          println("descending into: " + tree.printingPrefix + " " + tree)
            super.transform(tree)
        }
      }

    }

    /*
     * Casts a `tree` to a given type `tpe`.
     * XXX : Încropeală
     */
    private def cast(tree: Tree, tpe: Type) = {
      gen.mkAsInstanceOf(tree, tpe, true, false)
    }

    /*
     * Returns the code to box around some tree 
     */
    private def box(tree: Tree) = localTyper.typed(gen.mkMethodCall(minibox2box, List(tree)))

    /*
     * Tells whether a tree computes a value that will be stored on Long and
     * will have a Manifest[T].
     */
    private def isMiniboxed(t: Tree): Boolean = isMiniboxed(t.tpe)
    private def isMiniboxed(typ: Type): Boolean = isMiniboxed(typ.typeSymbol)
    private def isMiniboxed(typs: Symbol): Boolean = typs hasAnnotation MinispecedClass

    private def isMiniboxedArray(qual: Tree): Boolean =
      qual.tpe.underlying.typeArgs exists isMiniboxed

  }

}
