package miniboxing.plugin

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
    rootMirror.getRequiredModule("miniboxing.runtime.MiniboxTypeTagDispatch")
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
  private lazy val tag_== =
    definitions.getMember(TagDipatchObjectSymbol, newTermName("eqeq"))

  private lazy val ConversionsObjectSymbol =
    rootMirror.getRequiredModule("miniboxing.runtime.MiniboxConversions")
  private lazy val minibox2box =
    definitions.getMember(ConversionsObjectSymbol, newTermName("minibox2box"))
  private lazy val box2minibox =
    definitions.getMember(ConversionsObjectSymbol, newTermName("box2minibox"))

  private lazy val MiniboxArrayObjectSymbol =
    rootMirror.getRequiredModule("miniboxing.runtime.MiniboxArray")
  private lazy val newArray =
    definitions.getMember(MiniboxArrayObjectSymbol, newTermName("newArray"))
  private lazy val internal_newArray =
    definitions.getMember(MiniboxArrayObjectSymbol, newTermName("internal_newArray"))

  /**
   * The tree transformer that adds the trees for the specialized classes inside
   * the current package.
   */
  class MiniboxTreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = {
      curTree = tree

      // make sure specializations have been performed
      tree match {
        case t: SymTree => t.symbol.info
        case _ =>
      }

      tree match {
        /*
         *  We have created just the symbols for the specialized classes - now
         *  it's time to create their trees as well (initially empty).
         */
        case PackageDef(pid, classdefs) =>

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
          //println(tree.symbol + " ===> " + memberSpecializationInfo.apply(tree.symbol))
          memberSpecializationInfo.apply(tree.symbol) match {

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

            // forward to the target methods, making casts as prescribed
            case ForwardTo(target, retCast, paramCasts) =>
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
     */
    private def cast(tree: Tree, tpe: Type, cinfo: CastInfo) = {
      val ttree = ltypedpos(tree)
      cinfo match {
        case NoCast => ttree
        case AsInstanceOfCast => gen.mkAsInstanceOf(ttree, tpe, true, false)

        case CastMiniboxToBox(tag) =>
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
      for (m <- sClass.info.decls if m hasFlag MINIBOXED) {
        debug("creating empty tree for " + m.fullName)
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
    private def addBody(defn: Tree, original: Symbol) =
      defn match {
        case v: ValDef => addValDefBody(v, original)
        case d: DefDef => addDefDefBody(d, original)
      }

    private def addDefDefBody(defn: Tree, origMember: Symbol): Tree = {
      val defSymbol = defn.symbol
      val (origBody, origParams) = MethodBodiesCollector.getMethodBody(origMember);
      val origClass = origMember.owner

      /*
       * Most of the work of the tree transformer is done here.
       * We need to adapt the body of the generic class to use the value
       * representation of the current (specialized) class.
       *
       * In order to achieve this we must:
       * - insert type tag dispatching methods instead of the methods
       *   from `Array` and `Any` classes
       * - rewire method calls to use the overloads specialized for this
       *   representation
       * - replace all method selections to use the interface rather than
       *   the generic class
       * - insert conversions between boxed, miniboxed and natural representation
       *   of primitive values.
       * - redirect super calls 
       */
      var newBody = origBody.duplicate
      newBody = adaptTypes(newBody)
      newBody = (new replaceLocalCalls(currentClass, origClass))(newBody)

//      // debugging
//      val printtypes = settings.printtypes.value
//      settings.printtypes.value = true
//      println(defSymbol + ":\n" + asString(newBody))
//      settings.printtypes.value = printtypes
//      // end debugging

      duplicator.retypedMethod(
        localTyper.context1.asInstanceOf[duplicator.Context],
        copyDefDef(defn)(rhs = newBody),
        origMember.enclClass,
        defSymbol.enclClass,
        typeEnv(defSymbol.owner)) // XXX: keep all parameters
    }

    private def addValDefBody(defn: Tree, origMember: Symbol): Tree = {
      val defSymbol = defn.symbol
      val (origBody, origParams) = MethodBodiesCollector.getMethodBody(origMember);
      val origClass = origMember.owner

      var newBody = origBody.duplicate
      newBody = (new replaceLocalCalls(currentClass, origClass))(newBody)
      newBody = adaptTypes(newBody)

      duplicator.retyped(
        localTyper.context1.asInstanceOf[duplicator.Context],
        copyValDef(defn)(rhs = newBody),
        origMember.enclClass,
        defSymbol.enclClass,
        typeEnv(defSymbol.owner)) // XXX: keep all parameters
    }

    object duplicator extends {
      val global: MiniboxTreeTransformation.this.global.type =
        MiniboxTreeTransformation.this.global
    } with Duplicators

    /**
     * In the `_J` class, we should not be calling the generic methods, but
     * the `_J` version of it. Replace the symbol for the methods in the generic
     * class with the symbols of the corresponding methods in the interface.
     */
    private class replaceLocalCalls(sClass: Symbol, clazz: Symbol) extends Transformer {
      val spec: PartialSpec = partialSpec(sClass)
      def apply(tree: Tree): Tree = transform(tree)
      override def transform(tree: Tree): Tree = {
        val mbr = tree.symbol
        logTree("before", tree)
        val result = tree match {
          /*
           * `Select` nodes use the symbols for methods from the original class.
           * Change them to use the interface.
           *
           * TODO: do the same in the generic class.
           */
          case Select(obj, meth) if (mbr.owner == clazz && mbr.isMethod) =>
            debug(" *** " + meth + " : " + sClass)
            val iface = specializedInterface(clazz)
            // use the most specific overload
            val methName =
              if ((overloads isDefinedAt mbr) && overloads(mbr)(spec) != mbr)
                overloads(mbr)(spec).name
              else
                meth
            debug("  *  " + methName)

            // Mr. Typer will insert an unwanted Apply node here in case of no-params functions
            // No thanks Mr. Typer, keep them to yourself!
            typed(Select(obj, iface.tpe.decl(methName))) match {
              case Apply(tree, List()) => tree
              case tree => tree 
            }

          case _ => super.transform(tree)
        }
        logTree("after", result)
        result
      }


//      override def transform(tree: Tree): Tree = {
//        val mbr = tree.symbol
//        tree match {
//          /*
//           * `Select` nodes use the symbols for methods from the original class.
//           * Change them to use the interface.
//           *
//           * TODO: do the same in the generic class.
//           */
//          case Select(obj, meth) if mbr.owner == clazz =>
//            val newBody = 
//              if (mbr.isMethod) {
//                debug(" *** " + meth + " : " + sClass)
//                val iface = specializedInterface(clazz)
//                // use the most specific overload
//                val methName =
//                  if ((overloads isDefinedAt mbr) && overloads(mbr)(spec) != mbr)
//                    overloads(mbr)(spec).name
//                  else
//                    meth
//                debug("  *  " + methName)
//                Select(transform(obj), iface.tpe.decl(methName))
//              } else
//                Select(transform(obj), sClass.tpe.decl(meth))
//
//            typed(newBody)
//          case _: Select | _: Ident | _: This => 
//            tree.tpe = null
//            typed(tree)
//          case _ => super.transform(tree)
//        }
//      }
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

          /*
           * Array creation in miniboxed code is written by the user as:
           *   MiniboxArray.newArray[T](len)
           * and we rewrite it to:
           *   MiniboxArray.internal_newArray(len, tagOfT)
           */
          case Apply(TypeApply(meth, tpe :: Nil), len :: Nil) if (tree.symbol == newArray) =>
            debug("here " + tree.symbol);
            localTyper.typedPos(tree.pos)(
              gen.mkMethodCall(internal_newArray, List(transform(len), getTag(tpe))))

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
                  } else if (methodSym == Any_== && args.size == 1 && isMiniboxed(args(0))) {
                    if (qual.tpe.typeSymbol == args(0).tpe.typeSymbol) {
                      // the types are known to be equal
                      newTree = gen.mkMethodCall(tag_==,
                        asMB(transform(qual)) :: asMB(transform(args(0))) :: Nil)
                    } else {
                      newTree = gen.mkMethodCall(tag_==,
                        packedMB(transform(qual)) ::: packedMB(args(0)))
                    }
                  } else {
                    newTree = gen.mkMethodCall(box(transform(qual)), methodSym, Nil, newArgs)
                  }
                }

                /*
                 * TODO: For array access instructions, use the static methods from
                 * MiniboxTypeTagDispatch
                 */
                if (methodSym == Array_apply && isMiniboxedArray(qual)) {
                  val TypeApply(Select(qual1, _), _) = qual // get rid of the cast
                  val pos = newArgs(0)
                  newTree = gen.mkMethodCall(array_apply,
                    transform(qual1) :: pos :: miniboxedArrayTag(qual) :: Nil)
                }
                if (methodSym == Array_update && isMiniboxedArray(qual)) {
                  val TypeApply(Select(qual1, _), _) = qual // get rid of the cast
                  val pos = newArgs(0)
                  val elem = newArgs(1)
                  newTree = gen.mkMethodCall(array_update,
                    transform(qual1) :: pos :: asMB(elem) :: miniboxedArrayTag(qual) :: Nil)
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

      private def miniboxedArrayTag(qual: Tree) = {
        val currentClass = MiniboxTreeTransformer.this.currentClass
        val tparam = qual.tpe.underlying.typeArgs(0).typeSymbol
        val tag = typeTags(currentClass)(tparam)
        localTyper.typed(gen.mkAttributedRef(tag))
      }

      private def getTag(tpe: Tree) = {
        val currentClass = MiniboxTreeTransformer.this.currentClass
        val tparam = tpe.symbol
        val tag = typeTags(currentClass)(tparam)
        localTyper.typed(gen.mkAttributedRef(tag))
      }

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

      private def asMB(t: Tree) = {
        gen.mkAsInstanceOf(t, LongClass.tpe, true, false)
      }

    }

  }

}
