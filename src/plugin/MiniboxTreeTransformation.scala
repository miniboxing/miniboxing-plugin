package plugin

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.TypingTransformers
import scala.collection.mutable

import scala.tools.nsc.typechecker._

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
  private lazy val box2minibox =
    definitions.getMember(ConversionsObjectSymbol, newTermName("box2minibox"))

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
          //          println("class: " + tree.symbol.owner)
          //          println("info: " + tree.symbol + " " + memberSpecializationInfo(tree.symbol))
          memberSpecializationInfo(tree.symbol) match {

            // Implement the getter or setter functionality
            case FieldAccessor(field) =>
              val rhs1 = ltypedpos(
                if (tree.symbol.isGetter) {
                  gen.mkAttributedRef(field)
                } else {

                  Assign(gen.mkAttributedRef(field),
                    ltypedpos(Ident(vparamss.head.head.symbol)))
                })
              localTyper.typed(deriveDefDef(tree)(_ => rhs1))

            // copy the body of the `original` method
            case SpecializedImplementationOf(original) =>
              val newTree = addBody(tree, original)
              localTyper.typedPos(tree.pos)(newTree)

            case ForwardTo(target, retCast, paramCasts) => // XXX: cast info here
              val rhs1 = vparamss match {
                case Nil => gen.mkAttributedRef(target)
                case vparams :: _ =>
                  val params1 =
                    ((vparams zip target.paramss.head) zip paramCasts) map {
                      case ((p, t), paramCast) =>
                        cast(Ident(p.symbol), t.tpe, paramCast)
                    }
                  gen.mkMethodCall(target, tparams map (_.tpe), params1)
              }
              localTyper.typed(deriveDefDef(tree)(_ => cast(rhs1, tpt.tpe, retCast)))

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
              val newTree = addBody(tree, original)
              localTyper.typedPos(tree.pos)(newTree)
            case info =>
              sys.error("Unknown info type: " + info)
          }

        case DefDef(mods, name, tparams, vparamss, tpt, body) if (tree.symbol.isConstructor &&
          tree.symbol.paramss.head.size != vparamss.head.size) =>
          localTyper.typedPos(tree.pos)(DefDef(tree.symbol, _ => body))

        case _ =>
          //          println("descending into: " + tree.printingPrefix + " " + tree)
          super.transform(tree)
      }
    }

    def ltypedpos(tree: Tree): Tree =
      localTyper.typedPos(curTree.pos)(tree)

    def ltyped(tree: Tree): Tree =
      localTyper.typed(tree)

    /*
     * Casts a `tree` to a given type `tpe` as result of a `ForwardTo`
     * information being processed. The cast is perform as indicated by 
     * the `cinfo`.
     * 
     * XXX: make minibox2box use the right type argument
     */
    private def cast(tree: Tree, tpe: Type, cinfo: CastInfo) = {
      val ttree = ltypedpos(tree)
      cinfo match {
        case NoCast => ttree
        case AsInstanceOfCast => gen.mkAsInstanceOf(ttree, tpe, true, false)
        
        case CastMiniboxToBox(tag) => // use `tag` for miniboxing 
          val tagref = localTyper.typed(gen.mkAttributedRef(tag))
          ltypedpos(gen.mkMethodCall(minibox2box, List(ttree, tagref)))
        case CastBoxToMinibox =>
          ltypedpos(gen.mkMethodCall(box2minibox, List(ttree)))
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

            if (isSpecializableClass(classSymbol)) {
              var sClasses: List[Symbol] = Nil

              sClasses ::= specializedInterface(classSymbol)
              sClasses ++= specializedClasses(classSymbol)

              for (sClass <- sClasses) {
                debug("creating class - " + sClass.name + ": " + sClass.parentSymbols)
                val parents = sClass.info.parents map TypeTree
                buf +=
                  ClassDef(sClass, atPos(impl.pos)(Template(parents, emptyValDef, List()))
                    .setSymbol(sClass.newLocalDummy(sym1.pos))) setPos tree.pos
              }
            }
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
        case DefDef(_, _, _, vparams :: _, _, rhs) if (templateMembers(tree.symbol)) =>
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
     * Adds the body of the `member` as the rhs of the `defn` and
     * replaces the parameters with fresh symbols in it.
     */
    private def addBody(defn: Tree, origMember: Symbol): Tree = {
      val defSymbol = defn.symbol
      val (origBody, origParams) = MethodBodiesCollector.getMethodBody(origMember);

      val vparams: List[ValDef] = defn match {
        case DefDef(_, _, Nil, vparams :: Nil, _, _) => vparams
        case DefDef(_, _, Nil, Nil, _, _) => Nil
        case ValDef(_, _, _, _) => Nil
      }

      val newParams = cloneSymbolsAtOwner(vparams map (_.symbol), defSymbol)

      /*
       * Adapt the body to use miniboxed representation of fields where possible. 
       */
      var newBody = origBody 
      newBody = adaptTypes(newBody)
      newBody = (new replaceLocalCalls(currentClass))(newBody)
      newBody = (new TreeSymSubstituter(origParams, newParams take (origParams.size)))(newBody.duplicate)

      val newDef = defn match {
        case _: DefDef =>
          copyDefDef(defn)(vparamss = List(newParams map ValDef), rhs = newBody)
        case _: ValDef =>
          copyValDef(defn)(rhs = newBody)
      }

      println(defSymbol.fullName)
      /*
       * 
       */
      duplicator.retyped(
        localTyper.context1.asInstanceOf[duplicator.Context],
        newDef,
        origMember.enclClass,
        defSymbol.enclClass,
        typeEnv(defSymbol.owner)) // XXX: keep all parameters
    }

    object duplicator extends {
      val global: MiniboxTreeTransformation.this.global.type =
        MiniboxTreeTransformation.this.global
    } with Duplicators

    /**
     * Replace calls to generic functions with calls to specialized ones.
     */
    private class replaceLocalCalls(sClass: Symbol) extends Transformer {
      val spec: PartialSpec = partialSpec(sClass)
      def apply(tree: Tree): Tree = transform(tree)
      override def transform(tree: Tree): Tree = {
        val mbr = tree.symbol
        tree match {
          case Select(This(clazz), meth) if ((overloads isDefinedAt mbr) && overloads(mbr)(spec) != mbr) =>
            Select(This(sClass), overloads(mbr)(spec))
          case _ => super.transform(tree)
        }
      }
    }

    /**
     * When copying the code from the generic class to the specialized one,
     * we need to convert type parameters to the uniform representation: Long
     * and to replace code that treats them as instances of Any.
     *
     *
     * NOTE: In the standard implementation of specialization such casts
     * are done during erasure, but in our case we need to look at the type
     * Manifest when boxing/unboxing, so a custom transformation needs to be
     * done.
     */
    private object adaptTypes extends Transformer {
      def apply(tree: Tree): Tree = transform(tree)

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
              (methodSym.tpe.paramTypes zip transformTrees(args)) map {
                case (paramType, arg) =>
                  if (paramType == AnyClass.tpe && isMiniboxed(arg.tpe.typeSymbol))
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
                    newTree = gen.mkMethodCall(tag_##, packedMB(transform(qual)) ::: Nil)
                  } else if (methodSym == Any_hashCode) {
                    newTree = gen.mkMethodCall(tag_hashCode, packedMB(transform(qual)) ::: Nil)
                  } else {
                    newTree = gen.mkMethodCall(box(transform(qual)), methodSym, Nil, newArgs)
                  }
                }

                // XXX: use the tag in the other functions also
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

          // Insert a cast if necessary
          case Assign(lhs, rhs) if (lhs.tpe == AnyClass.tpe && isMiniboxed(rhs)) =>
            localTyper.typed(atPos(tree.pos)(Assign(lhs, box(transform(rhs)))))

          case _ =>
            //          println("descending into: " + tree.printingPrefix + " " + tree)
            super.transform(tree)
        }
      }
      /*
       * Returns the code to box around some tree 
       */
      private def box(tree: Tree) = localTyper.typed(
        gen.mkMethodCall(minibox2box, packedMB(tree)))

      /*
       * Tells whether a tree computes a value that will be stored on Long and
       * will have a type tag.
       */
      private def isMiniboxed(t: Tree): Boolean = isMiniboxed(t.tpe)
      private def isMiniboxed(typ: Type): Boolean = isMiniboxed(typ.typeSymbol)

      private def isMiniboxed(typs: Symbol): Boolean = {
        val currentClass = MiniboxTreeTransformer.this.currentClass
        partialSpec(currentClass).get(typs) match {
          case Some(Miniboxed) => true
          case _ => false
        }
      }

      private def isMiniboxedArray(qual: Tree): Boolean =
        qual.tpe.underlying.typeArgs exists isMiniboxed

      /*
       * Converts a tree that has type `T` whith Minispeced annotation to
       * a packed representation: a Long representing the value and a 
       * Byte representing the type.
       */
      private def packedMB(t: Tree) = {
        assert(isMiniboxed(t))
        val currentClass = MiniboxTreeTransformer.this.currentClass
        val tag = typeTags(currentClass)(t.tpe.typeSymbol)

        List(gen.mkAsInstanceOf(t, LongClass.tpe, true, false),
          localTyper.typed(gen.mkAttributedRef(tag)))
      }

    }

  }

}
