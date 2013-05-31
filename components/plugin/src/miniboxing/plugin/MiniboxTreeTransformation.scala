package miniboxing.plugin

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.TypingTransformers
import scala.collection.mutable
import scala.tools.nsc.typechecker._
import com.sun.org.apache.bcel.internal.util.ClassStack

trait MiniboxTreeTransformation extends TypingTransformers {
  self: MiniboxComponent =>

  import global._
  import definitions._
  import Flags._
  import typer.{ typed, atOwner }
  import memberSpecializationInfo._

  /** A tree symbol substituter that substitutes on type skolems.
   *  If a type parameter is a skolem, it looks for the original
   *  symbol in the 'from' and maps it to the corresponding new
   *  symbol. The new symbol should probably be a type skolem as
   *  well (not enforced).
   *
   *  All private members are made protected in order to be accessible from
   *  specialized classes.
   */
  class ImplementationAdapter(from: List[Symbol],
                              to: List[Symbol],
                              targetClass: Symbol,
                              addressFields: Boolean) extends TreeSymSubstituter(from, to) {
    override val symSubst = new SubstSymMap(from, to) {
      override def matches(sym1: Symbol, sym2: Symbol) =
        if (sym2.isTypeSkolem) sym2.deSkolemize eq sym1
        else sym1 eq sym2
    }

    private def isAccessible(sym: Symbol): Boolean =
      (currentClass == sym.owner.enclClass) && (currentClass != targetClass)

    private def shouldMakePublic(sym: Symbol): Boolean =
      sym.hasFlag(PRIVATE | PROTECTED) && (addressFields || !nme.isLocalName(sym.name))

    /** All private members that are referenced are made protected,
     *  in order to be accessible from specialized subclasses.
     */
    override def transform(tree: Tree): Tree = tree match {
      case Select(qual, name) =>
        val sym = tree.symbol
        if (sym.isPrivate) debuglog(
          "seeing private member %s, currentClass: %s, owner: %s, isAccessible: %b, isLocalName: %b".format(
            sym, currentClass, sym.owner.enclClass, isAccessible(sym), nme.isLocalName(sym.name))
        )
        if (shouldMakePublic(sym) && !isAccessible(sym)) {
          debuglog("changing private flag of " + sym)
          sym.makeNotPrivate(sym.owner)
        }
        super.transform(tree)

      case _ =>
        super.transform(tree)
    }
  }

  /**
   * The tree transformer that adds the trees for the specialized classes inside
   * the current package.
   */
  class MiniboxTreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    def reportError[T](body: =>T)(handler: TypeError => T): T =
      try body
      catch {
        case te: TypeError =>
          reporter.error(te.pos, te.msg)
          (new Exception()).printStackTrace()
          handler(te)
      }

    /** This duplicator additionally performs casts of expressions if that is allowed by the `casts` map. */
    class Duplicator(casts: Map[Symbol, Type]) extends {
      val global: MiniboxTreeTransformation.this.global.type = MiniboxTreeTransformation.this.global
      val miniboxing: MiniboxComponent { val global: MiniboxTreeTransformation.this.global.type } =
        MiniboxTreeTransformation.this.asInstanceOf[MiniboxComponent { val global: MiniboxTreeTransformation.this.global.type }]
    } with Duplicators {
      private val (castfrom, castto) = casts.unzip
      private object CastMap extends SubstTypeMap(castfrom.toList, castto.toList)

      class BodyDuplicator(_context: Context) extends super.BodyDuplicator(_context) {
        override def castType(tree: Tree, pt: Type): Tree = {
          // log(" expected type: " + pt)
          // log(" tree type: " + tree.tpe)
          tree.tpe = if (tree.tpe != null) fixType(tree.tpe) else null
          // log(" tree type: " + tree.tpe)
          val ntree = if (tree.tpe != null && !(tree.tpe <:< pt)) {
            val casttpe = CastMap(tree.tpe)
            if (casttpe <:< pt) gen.mkCast(tree, casttpe)
            else if (casttpe <:< CastMap(pt)) gen.mkCast(tree, pt)
            else tree
          } else tree
          ntree.tpe = null
          ntree
        }
      }

      protected override def newBodyDuplicator(context: Context) = new BodyDuplicator(context)
    }

    def typeTagTrees(symbol: Symbol) =
      localTypeTags(symbol).map({case (t, tag) => (t, Ident(tag))}) ++ globalTypeTags(symbol.owner).map({case (t, tag) => (t, Select(This(symbol.owner), tag))})

    import global._

    def afterMinibox[T](f: => T): T = atPhase(ownPhase.next)(f)

    override def transform(tree: Tree): Tree = miniboxTransform(tree)

    def miniboxTransform(tree: Tree): Tree = {
      curTree = tree
      // make sure specializations have been performed
      tree match {
        case t: SymTree => afterMinibox(t.symbol.info)
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
            val templates = transformStats(classdefs ::: specClasses, tree.symbol.moduleClass)
            val packageTree = treeCopy.PackageDef(tree, pid, templates)
            localTyper.typedPos(tree.pos)(packageTree)
          }

        /*
         * This is either a class that has nothing to do with miniboxing or that is the base
         * class (now trait) for the specialization.
         *
         * Also collect the bodies of the methods that need to be copied and specialized.
         */
        case Template(parents, self, body) if { afterMinibox(tree.symbol.enclClass.info); specializedBase(tree.symbol.enclClass) } =>
          MethodBodiesCollector(tree)
          val traitSym = tree.symbol.enclClass
          val traitDecls = afterMinibox(traitSym.info).decls.toList
          val traitCtor = localTyper.typed(DefDef(traitSym.primaryConstructor, Block(List(), Literal(Constant()))))
          val specMembers = createMethodTrees(tree.symbol.enclClass) map localTyper.typed
          localTyper.typedPos(tree.pos)(
            treeCopy.Template(tree, parents, self,
              atOwner(currentOwner)(transformTrees(traitCtor :: body.filter(defdef => traitDecls.contains(defdef.symbol)) ::: specMembers))))

        /*
         * The tree of a specialized class is empty for the moment, but we
         * create symbols for the methods - give them an empty body.
         */
        case Template(parents, self, body) =>
          val specMembers = createMethodTrees(tree.symbol.enclClass) map localTyper.typed
          val memberDefs = atOwner(currentOwner)(transformTrees(body ::: specMembers))
          val templateDef = treeCopy.Template(tree, parents, self, memberDefs)
          localTyper.typedPos(tree.pos)(templateDef)

        /*
         * The trait constructor -- which we leave empty as this is just a simple interface, nothing special about it
         */
        case ddef @ DefDef(mods, name, tparams, vparamss, tpt, _) if specializedBase(ddef.symbol.enclClass) && ddef.symbol.name != nme.MIXIN_CONSTRUCTOR =>
          localTyper.typed(treeCopy.DefDef(ddef, mods, name, tparams, vparamss = List() ::: vparamss, tpt, EmptyTree))

        /*
         * A definition with empty body - add a body as prescribed by the
         * `methodSpecializationInfo` data structure.
         */
        case ddef @ DefDef(mods, name, tparams, vparamss, tpt, EmptyTree) if hasInfo(ddef) =>
          memberSpecializationInfo.apply(tree.symbol) match {
            // Implement the getter or setter functionality
            case FieldAccessor(field) =>
              val localTypeArgs = localTypeTags(tree.symbol)
              val allArgs = tree.symbol.tpe.paramss.flatten
              val mthArgs = allArgs.drop(localTypeArgs.size)
              val rhs1 = ltypedpos(
                if (tree.symbol.isGetter) {
                  gen.mkAttributedRef(field)
                } else {
                  Assign(gen.mkAttributedRef(field),
                    ltypedpos(Ident(mthArgs.head)))
                })
              localTyper.typed(deriveDefDef(tree)(_ => rhs1))

            // forward to the target methods, making casts as prescribed
            case ForwardTo(ttagArgs, target, retCast, paramCasts) =>

              def separateTypeTagArgsInTree(args: List[List[Tree]]): (List[Tree], List[List[Tree]]) = {
                args match {
                  case ttargs :: rest if ttargs.forall(_.symbol.name.toString.contains("_TypeTag")) =>
                    (ttargs, rest)
                  case _ =>
                    (Nil, args)
                }
              }

              def separateTypeTagArgsInType(tpe: Type): (List[Symbol], Type) =
                tpe match {
                  case MethodType(ttargs, tpe) if ttargs.forall(_.name.toString.contains("_TypeTag")) =>
                    (ttargs, tpe)
                  case _ =>
                    (Nil, tpe)
                }

              val (ttagWrapperArgs, paramss) = separateTypeTagArgsInTree(vparamss)
              val (ttagFormalArgs, targetTpe) = separateTypeTagArgsInType(target.tpe)
              val targetParams = targetTpe.paramss match {
                case pars :: _ => pars
                case _ => Nil
              }

              def callWithTypeTags = {
                ttagArgs match {
                  case Nil =>
                    gen.mkAttributedRef(target)
                  case _ =>
                    gen.mkMethodCall(target, ttagArgs.map(gen.mkAttributedRef(_)))
                }
              }

              val rhs1 = paramss match {
                case Nil =>
                  callWithTypeTags
                case vparams :: _ =>
                  val params1 =
                    ((vparams zip targetParams) zip paramCasts) map {
                      case ((p, t), paramCast) =>
                        cast(Ident(p.symbol), t.tpe, paramCast)
                    }
                  gen.mkMethodCall(callWithTypeTags, params1)
              }

              localTyper.typed(deriveDefDef(tree)(_ => cast(rhs1, tpt.tpe, retCast)))

            // copy the body of the `original` method
            case SpecializedImplementationOf(target) =>
              // we have an rhs, specialize it
              def reportTypeError(body: =>Tree) = reportError(body)(_ => ddef)
//              val tree1 = reportTypeError {
              val tree1 = duplicateBody(ddef, target)
//              }
              debuglog("implementation: " + tree1)
              deriveDefDef(tree1)(transform)

            case Interface() =>
              tree

            case OverrideOfSpecializedMethod(target) =>
              sys.error("Not yet implemented!")

//            case info =>
//              localTyper.typed(treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt, localTyper.typed(Block(Ident(Predef_???)))))
//              sys.error("Unknown info type: " + info)
          }

        case vdef @ ValDef(mods, name, tpt, EmptyTree) if hasInfo(vdef) =>
          memberSpecializationInfo(tree.symbol) match {
            case SpecializedImplementationOf(original) =>
              val newTree = addValDefBody(tree, original)
              localTyper.typedPos(tree.pos)(newTree)
            case info =>
              sys.error("Unknown info type: " + info)
          }

        case DefDef(mods, name, tparams, vparamss, tpt, body) if (tree.symbol.isConstructor &&
          tree.symbol.paramss.head.size != vparamss.head.size) =>
          debug(" => overriding constructor in " + tree.symbol.ownerChain.reverse.map(_.nameString).mkString(".") + ":\n" + tree)
          val result = localTyper.typedPos(tree.pos)(DefDef(tree.symbol, _ => body))
          debug(" <= " + result)
          result

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
            afterMinibox(tree.symbol.info)
            val classSymbol = tree.symbol

            if (isSpecializableClass(classSymbol)) {
              var sClasses: List[Symbol] = Nil

              sClasses ++= specializedClasses(classSymbol)

              for (sClass <- sClasses) {
                debug("creating class - " + sClass.name + ": " + sClass.parentSymbols)
                val parents = sClass.info.parents map TypeTree
                buf +=
                  ClassDef(sClass, atPos(impl.pos)(Template(parents, emptyValDef, List()))
                    .setSymbol(sClass.newLocalDummy(classSymbol.pos))) setPos tree.pos
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
      private val body = mutable.HashMap[Symbol, (Tree, List[List[Symbol]])]()

      override def traverse(tree: Tree) = tree match {
        case DefDef(_, _, _, vparamss, _, rhs) if (templateMembers(tree.symbol)) =>
          collect(tree.symbol, rhs, vparamss.map(_.map(_.symbol)))
        case DefDef(_, _, _, Nil, _, rhs) if (templateMembers(tree.symbol)) =>
          collect(tree.symbol, rhs, Nil)
        case ValDef(mods, name, tpt, rhs) if templateMembers(tree.symbol) =>
          collect(tree.symbol, rhs, Nil)
        case _ =>
          super.traverse(tree)
      }

      private def collect(member: Symbol, rhs: Tree, params: List[List[Symbol]]) = {
        body(member) = (rhs, params)
        templateMembers -= member
        debug("collected " + member.fullName)
      }

      def getMethodBody(meth: Symbol) = body(meth)
      def getFieldBody(fld: Symbol) = body(fld)._1
    }

    /** Duplicate the body of the given method `tree` to the new symbol `source`.
     *
     *  Knowing that the method can be invoked only in the `castmap` type environment,
     *  this method will insert casts for all the expressions of types mappend in the
     *  `castmap`.
     */
    private def duplicateBody(tree: DefDef, source: Symbol, castmap: TypeEnv = Map.empty) = {
      val symbol = tree.symbol
      val meth = addBody(tree, source)

      val d = new Duplicator(castmap)
      debuglog("-->d DUPLICATING: " + meth)
      d.retyped(
        localTyper.context1.asInstanceOf[d.Context],
        meth,
        source.enclClass,
        symbol.enclClass,
        typeEnv(symbol.owner).deepEnv,
        typeEnv(symbol.owner).shallowEnv,
        miniboxedArgs(symbol),
        typeTagTrees(symbol)
      )
    }

    /** Put the body of 'source' as the right hand side of the method 'tree'.
     *  The destination method gets fresh symbols for type and value parameters,
     *  and the body is updated to the new symbols, and owners adjusted accordingly.
     *  However, if the same source tree is used in more than one place, full re-typing
     *  is necessary. @see method duplicateBody
     */
    private def addBody(tree: DefDef, source: Symbol): DefDef = {
      val symbol = tree.symbol
      debuglog("specializing body of " + symbol.defString)
      val (tparams, tags, vparamss, tpt) = tree match {
        case DefDef(_, _, tparams, tags :: vparamss, tpt, _) if base.getOrElse(symbol, NoSymbol) != symbol =>
          (tparams, tags, vparamss, tpt)
        case DefDef(_, _, tparams, vparamss, tpt, _) if base.getOrElse(symbol, NoSymbol) == symbol =>
          (tparams, Nil, vparamss, tpt)
      }
      val env = typeEnv(symbol.owner).deepEnv // TODO
      val boundTvars = env.keySet
      val origtparams = source.typeParams.filter(tparam => !boundTvars(tparam) || !isPrimitiveValueType(env(tparam)))
      if (origtparams.nonEmpty || symbol.typeParams.nonEmpty)
        debuglog("substituting " + origtparams + " for " + symbol.typeParams)

      // skolemize type parameters
      val oldtparams = tparams map (_.symbol)
      val newtparams = deriveFreshSkolems(oldtparams)
      map2(tparams, newtparams)(_ setSymbol _)

      // create fresh symbols for value parameters to hold the skolem types
      // val newSyms = cloneSymbolsAtOwnerAndModify(vparamss.flatten.map(_.symbol), symbol, _.substSym(oldtparams, newtparams))

      // replace value and type parameters of the old method with the new ones
      // log("Adding body for " + tree.symbol + " - origtparams: " + origtparams + "; tparams: " + tparams)
      // log("Type vars of: " + source + ": " + source.typeParams)
      // log("Type env of: " + tree.symbol + ": " + boundTvars)
      // log("newtparams: " + newtparams)
      val (body, parameters) = MethodBodiesCollector.getMethodBody(source)

      val symSubstituter = new ImplementationAdapter(
        parameters.flatten ::: origtparams,
        vparamss.flatten.map(_.symbol) ::: newtparams,
        source.enclClass,
        false) // don't make private fields public

      val newBody = symSubstituter(body.duplicate)
      tpt.tpe = tpt.tpe.substSym(oldtparams, newtparams)

      copyDefDef(tree)(rhs = newBody)
    }

//    private def addDefDefBody(defn: Tree, origMember: Symbol): Tree = {
//      // original member
//      val (origBody, origParams) = MethodBodiesCollector.getMethodBody(origMember);
//      val origClass = origMember.owner
//      // specialized member
//      val specMember = defn.symbol
//
//      /*
//       * There are a couple of transformations that we need to perform to specialize the code. All of these
//       * transformations should leave the tree behaving the same as before and should keep its types consistent
//       *
//       * (1) transform all references to miniboxed parameters into calls to `minibox2box[T](longValue, typeTag)`
//       *     TODO: Do we need to do anything about object parameters?
//       * (2) shortcut array operations, Any operations (like hashCode) to the non-boxing variants
//       * (3) rewire calls to specialized overloads instead of the generic methods
//       *     rewire member accesses to access the specialized members
//       * (4) rewire calls to go against the interface instead of the class
//       *     TODO: Decide whether we really need this
//       * (5) create new symbols in the tree for labels, locally-defined values etc.
//       * (6) rewire super calls
//       */
//      var newBody = origBody.duplicate
//      newBody = duplicator.retypedMethod(
//        context = localTyper.context1.asInstanceOf[duplicator.Context],
//        tree = copyDefDef(defn)(rhs = newBody),
//        oldThis = origMember.enclClass,
//        newThis = specMember.enclClass)
//        // env = typeEnv(specMember.owner).deepEnv) // XXX: keep all parameters
//
//      newBody
//
////      /*
////       * Most of the work of the tree transformer is done here.
////       * We need to adapt the body of the generic class to use the value
////       * representation of the current (specialized) class.
////       *
////       * In order to achieve this we must:
////       * - insert type tag dispatching methods instead of the methods
////       *   from `Array` and `Any` classes
////       * - rewire method calls to use the overloads specialized for this
////       *   representation
////       * - replace all method selections to use the interface rather than
////       *   the generic class
////       * - insert conversions between boxed, miniboxed and natural representation
////       *   of primitive values.
////       * - redirect super calls
////       */
////      var newBody = origBody.duplicate
////      newBody = adaptTypes(newBody)
////
//////      if (defn.symbol.name.toString == "contains_J")
//////        logTree(defSymbol.fullName + " before: ", origBody)
//////      // debugging
//////      val printtypes = settings.printtypes.value
//////      settings.printtypes.value = true
//////      println(defSymbol + ":\n" + asString(newBody))
//////      settings.printtypes.value = printtypes
//////      // end debugging
////
////      newBody = propagateMiniboxedParameters(newBody)
////
////
////        val res = (new replaceLocalCalls(currentClass, origClass))( {
////          if (defn.symbol.name.toString == "contains_J")
////            logTree(defSymbol.fullName + " after: ", result)
////          result
////        }
////      )
////
////      if (defn.symbol.name.toString == "contains_J")
////        logTree(defSymbol.fullName + " finally: ", res)
////
////      res
//    }


    private def addValDefBody(tree: Tree, origMember: Symbol): Tree = {
      val defSymbol = tree.symbol
      val origBody = MethodBodiesCollector.getFieldBody(origMember);
      val origClass = origMember.owner

      val tree1 = deriveValDef(tree)(_ => origBody.duplicate)
      debuglog("now typing: " + tree1 + " in " + tree.symbol.owner.fullName)

      val d = new Duplicator(Map.empty)
      val newValDef = d.retyped(
        localTyper.context1.asInstanceOf[d.Context],
        tree1,
        origClass,
        defSymbol.enclClass,
        typeEnv(defSymbol.owner).deepEnv,
        typeEnv(defSymbol.owner).shallowEnv,
        Nil, // A field does not take values
        typeTagTrees(defSymbol)
      )
      deriveValDef(newValDef)(transform)
    }

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
        //logTree("before", tree)
        val result = tree match {
          /*
           * `Select` nodes use the symbols for methods from the original class.
           * Change them to use the interface.
           *
           * TODO: do the same in the generic class.
           */
//          case Select(obj, meth) if (mbr.owner == clazz && mbr.isMethod) =>
//            debug(" *** " + meth + " : " + sClass)
//            val iface = specializedInterface(clazz)
//            // use the most specific overload
//            val methName =
//              if ((overloads isDefinedAt mbr) && overloads(mbr)(spec) != mbr)
//                overloads(mbr)(spec).name
//              else
//                meth
//            debug("  *  " + methName)
//
//            // Mr. Typer will insert an unwanted Apply node here in case of no-params functions
//            // No thanks Mr. Typer, keep them to yourself!
//            typed(Select(obj, iface.tpe.decl(methName))) match {
//              case Apply(tree, List()) => tree
//              case tree => tree
//            }

          case _ => super.transform(tree)
        }
        //logTree("after", result)
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
           *   Manifest[T].newArray[T](len)
           * and we rewrite it to:
           *   MiniboxArray.internal_newArray(len, tagOfT)
           */
          case Apply(TypeApply(meth, tpe :: Nil), len :: Nil) if (tree.symbol == newArray) =>
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
        val tag = ??? //typeTags(currentClass)(tparam)
        localTyper.typed(gen.mkAttributedRef(tag))
      }

      private def getTag(tpe: Tree) = {
        val currentClass = MiniboxTreeTransformer.this.currentClass
        val tparam = tpe.symbol
        val tag = ??? //typeTags(currentClass)(tparam)
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
        val tag = ??? //typeTags(currentClass)(t.tpe.typeSymbol)

        List(gen.mkAsInstanceOf(t, LongClass.tpe, true, false),
          localTyper.typed(gen.mkAttributedRef(tag)))
      }

      private def asMB(t: Tree) = {
        gen.mkAsInstanceOf(t, LongClass.tpe, true, false)
      }

    }

  }

}
