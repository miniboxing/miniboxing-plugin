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
                              addressFields: Boolean,
                              makePublicAtAll: Boolean) extends TreeSymSubstituter(from, to) {
    override val symSubst = new SubstSymMap(from, to) {
      override def matches(sym1: Symbol, sym2: Symbol) =
        if (sym2.isTypeSkolem) sym2.deSkolemize eq sym1
        else sym1 eq sym2
    }

    private def isAccessible(sym: Symbol): Boolean =
      (currentClass == sym.owner.enclClass) && (currentClass != targetClass)

    private def shouldMakePublic(sym: Symbol): Boolean =
      makePublicAtAll && sym.hasFlag(PRIVATE | PROTECTED) && (addressFields || !nme.isLocalName(sym.name))

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

    def separateTypeTagArgsInTree(args: List[List[Tree]]): (List[Tree], List[List[Tree]]) = args match {
      case ttargs :: rest if ttargs.forall(_.symbol.name.toString.contains("_TypeTag")) => (ttargs, rest)
      case _ => (Nil, args)
    }

    def separateTypeTagArgsInType(tpe: Type): (List[Symbol], Type) = tpe match {
      case MethodType(ttargs, tpe) if ttargs.forall(_.name.toString.contains("_TypeTag")) => (ttargs, tpe)
      case _ => (Nil, tpe)
    }

    def typeTagTrees(symbol: Symbol = currentMethod) =
      localTypeTags.getOrElse(symbol, Map.empty).map({case (t, tag) => (t, Ident(tag))}) ++
      globalTypeTags.getOrElse((if (symbol != NoSymbol) symbol else currentClass), Map.empty).map({case (t, tag) => (t, gen.mkAttributedSelect(gen.mkAttributedThis(tag.owner),tag))}) ++
      standardTypeTagTrees

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
          super.transform(localTyper.typedPos(tree.pos)(
            treeCopy.Template(tree, parents, self,
              atOwner(currentOwner)(transformTrees(traitCtor :: body.filter(defdef => traitDecls.contains(defdef.symbol)) ::: specMembers)))))

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
          val res = memberSpecializationInfo.apply(tree.symbol) match {
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
              val (ttagWrapperArgs, paramss) = separateTypeTagArgsInTree(vparamss)
              val (ttagFormalArgs, targetTpe) = separateTypeTagArgsInType(target.tpe)
              val targetParams = targetTpe.paramss match {
                case pars :: _ => pars
                case _ => Nil
              }

              def callWithTypeTags =
                ttagArgs match {
                  case Nil =>  gen.mkAttributedRef(target)
                  case _ => gen.mkMethodCall(target, ttagArgs.map(gen.mkAttributedRef(_)))
                }

              val rhs1 = paramss match {
                case Nil =>  callWithTypeTags
                case vparams :: _ =>
                  val params1 =
                    ((vparams zip targetParams) zip paramCasts) map {
                      case ((p, t), paramCast) =>
                        cast(Ident(p.symbol), t.tpe, paramCast)
                    }
                  gen.mkMethodCall(callWithTypeTags, params1)
              }

              super.transform(localTyper.typed(deriveDefDef(tree)(_ => cast(rhs1, tpt.tpe, retCast))))

            // copy the body of the `original` method
            case SpecializedImplementationOf(target) =>
              // we have an rhs, specialize it
              def reportTypeError(body: =>Tree) = reportError(body)(_ => ddef)
              val tree1 = specializeDefDefBody(ddef, target)
              debuglog("implementation: " + tree1)
              tree1

            case Interface() =>
              tree

            case info =>
              super.transform(localTyper.typed(treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt, localTyper.typed(Block(Ident(Predef_???))))))
              sys.error("Unknown info type: " + info)
          }
          super.transform(res)

        case vdef @ ValDef(mods, name, tpt, EmptyTree) if hasInfo(vdef) =>
          memberSpecializationInfo(tree.symbol) match {
            case SpecializedImplementationOf(original) =>
              val newTree = addValDefBody(tree, original)
              super.transform(localTyper.typedPos(tree.pos)(newTree))
            case info =>
              sys.error("Unknown info type: " + info)
          }

        case DefDef(mods, name, tparams, vparamss, tpt, body) if (tree.symbol.isConstructor &&
          tree.symbol.paramss.head.size != vparamss.head.size) =>
          debug(" => overriding constructor in " + tree.symbol.ownerChain.reverse.map(_.nameString).mkString(".") + ":\n" + tree)
          val result = localTyper.typedPos(tree.pos)(DefDef(tree.symbol, _ => body))
          debug(" <= " + result)
          super.transform(result)

        case Apply(sel @ Select(qual, fn), args) if { afterMinibox(sel.symbol.owner.info); base.isDefinedAt(tree.symbol) && base(tree.symbol) == tree.symbol } =>
          val oldMethodSym = tree.symbol
          val oldMethodType = sel.tpe
          val tree1 =
            memberSpecializationInfo.get(currentMethod) match {
              case Some(spec: ForwardTo) =>
                // don't touch forwarders, they're correctly generated the first time
                // although it may be interesting to use this logic to generate forwarders too
                // TODO: Can this be done?
                tree
              case _ =>
                extractSpec(tree, qual.tpe) match { // Get the partial specialization
                  case Some((pspec, tagTrees)) if !isAllAnyRef(pspec) && overloads.get(oldMethodSym).flatMap(_.get(pspec)).isDefined =>
                    // println("\n\n")
                    // println("    FROM: " + oldMethodSym.defString)
                    val newMethodSym = overloads(oldMethodSym)(pspec)
                    // println("    TO:   " + newMethodSym.defString)
                    // println(pspec + "  " + tagTrees + " ==> " + newMethodSym.defString)
                    val tree1 = rewiredMethodCall(qual, oldMethodSym, oldMethodType, newMethodSym, args, pspec, tagTrees ++ standardTypeTagTrees)
                    stats("redirecting new: " + tree + " ==> " + tree1)
                    tree1
                  case other =>
                    tree
                }
            }
          super.transform(tree1)

        case Select(qual, fn) if { afterMinibox(tree.symbol.owner.info); base.isDefinedAt(tree.symbol) && base(tree.symbol) == tree.symbol } =>
          val oldMethodSym = tree.symbol
          val oldMethodType = tree.tpe
          val tree1 =
            memberSpecializationInfo.get(currentMethod) match {
              case Some(spec: ForwardTo) =>
                // don't touch forwarders, they're correctly generated the first time
                // although it may be interesting to use this logic to generate forwarders too
                // TODO: Can this be done?
                tree
              case _ =>
              extractSpec(tree, qual.tpe) match { // Get the partial specialization
                  case Some((pspec, tagTrees)) if !isAllAnyRef(pspec) && overloads.get(oldMethodSym).flatMap(_.get(pspec)).isDefined =>
                    // println("\n\n")
                    // println("    FROM: " + oldMethodSym.defString)
                    val newMethodSym = overloads(oldMethodSym)(pspec)
                    // println("    TO:   " + newMethodSym.defString)
                    // println(pspec + "  " + tagTrees + " ==> " + newMethodSym.defString)
                    val tree1 = rewiredMethodCall(qual, oldMethodSym, oldMethodType, newMethodSym, null, pspec, tagTrees ++ standardTypeTagTrees)
                    stats("redirected selection: " + tree + " ==> " + tree1)
                    tree1
                  case other =>
                    tree
                }
            }
          super.transform(tree1)

        case Apply(ctor @ Select(qual @ New(cl), nme.CONSTRUCTOR), args) if { afterMinibox(cl.symbol.info); specializedClasses.isDefinedAt(qual.tpe.typeSymbol) } =>
          val oldClassCtor = ctor.symbol
          val tree1 = cl.tpe match {
            case TypeRef(pre, oldClass, targs) =>
              extractSpec(tree, cl.tpe, true) match {
                case Some((pspec, tagTrees)) if !isAllAnyRef(pspec) =>
                  assert(specializedClasses(oldClass).isDefinedAt(pspec) && overloads.isDefinedAt(ctor.symbol) && overloads(ctor.symbol).isDefinedAt(pspec))
                  // println("\n\n")
                  val newClass = specializedClasses(oldClass)(pspec)
                  val newClassCtor = overloads(oldClassCtor)(pspec)
                  val newQual = New(TypeTree(TypeRef(pre, newClass, targs)))
                  // println("    FROM: " + oldClass.defString)
                  // println("    TO:   " + newClass.defString)
                  // println("redirect to: " + newClass)
                  // println(pspec + "  " + tagTrees + " ==> " + newClassCtor.defString)
                  // println()
                  val tree1 = rewiredMethodCall(newQual, oldClassCtor, ctor.tpe, newClassCtor, args, pspec, tagTrees ++ standardTypeTagTrees, tagTranslator = typeParamMap(newClass))
                  stats("redirecting new: " + tree + " ==> " + tree1)
                  tree1
                case Some(_) =>
                  val allAnyRefSpec = oldClass.typeParams.map(t => (t, Boxed)).toMap
                  val newClass = specializedClasses(oldClass)(allAnyRefSpec)
                  val newClassCtor = overloads(oldClassCtor)(allAnyRefSpec)
                  val newQual = New(TypeTree(TypeRef(pre, newClass, targs)))
                  gen.mkMethodCall(gen.mkMethodCall(newQual, newClassCtor, List(), List()), args)
                case None =>
                  global.reporter.error(tree.pos, "Unable to rewire constructor, this will probably lead to invalid bytecode.")
                  tree
              }
            case _ =>
              global.reporter.error(tree.pos, "Unsupported new operation.")
              tree
          }
          super.transform(localTyper.typed(tree1))

        // Array application
        case Apply(apply @ Select(array, _), List(pos)) if apply.symbol == Array_apply =>
          val tags = typeTagTrees()
          val tree1 = array.tpe.widen.typeArgs match {
            case tpe :: Nil if tags.isDefinedAt(tpe.typeSymbol) =>
              val tag = tags(tpe.typeSymbol)
              val tree1 = gen.mkMethodCall(mbarray_apply, List(array, pos, tag))
              val tree2 = gen.mkMethodCall(minibox2box, List(tpe), List(tree1, tag))
              stats("rewrote array apply: " + tree + " ==> " + tree2)
              tree2
            case _ =>
              tree
          }
          super.transform(localTyper.typed(tree1))

        // Array update
        case Apply(update @ Select(array, _), List(pos, element)) if update.symbol == Array_update =>
          val tags = typeTagTrees()
          val tree1 = array.tpe.widen.typeArgs match {
            case tpe :: Nil if tags.isDefinedAt(tpe.typeSymbol) =>
              val tag = tags(tpe.typeSymbol)
              val tree1 = gen.mkMethodCall(box2minibox, List(tpe), List(element, tag))
              val tree2 = gen.mkMethodCall(mbarray_update, List(array, pos, tree1, tag))
              stats("rewrote array update: " + tree + " ==> " + tree2)
              tree2
            case _ =>
              tree
          }
          super.transform(localTyper.typed(tree1))

        // Array new
        case Apply(newArray @ Select(manifest, _), List(size)) if newArray.symbol == Manifest_newArray =>
          val tags = typeTagTrees()
          val tree1 = manifest.tpe.widen.typeArgs match {
            case tpe :: Nil if tags.isDefinedAt(tpe.typeSymbol) =>
              val tag = tags(tpe.typeSymbol)
              val tree1 = gen.mkMethodCall(mbarray_new, List(size, tag))
              //val tree2 = gen.mkAsInstanceOf(tree1, tree.tpe)
              stats("rewrote array new: " + tree + " ==> " + tree1)
              tree1
            case _ =>
              tree
          }
          super.transform(localTyper.typed(tree1))

        // Array length
        case length @ Select(array, _) if length.symbol == Array_length =>
          val tags = typeTagTrees()
          val tree1 = array.tpe.widen.typeArgs match {
            case tpe :: Nil if tags.isDefinedAt(tpe.typeSymbol) =>
              val tag = tags(tpe.typeSymbol)
              val tree1 = gen.mkMethodCall(mbarray_length, List(array, tag))
              stats("rewrote array length: " + tree + " ==> " + tree1)
              tree1
            case _ =>
              tree
          }
          super.transform(localTyper.typed(tree1))

        case _ =>
          super.transform(tree)
      }
    }

    def extractSpec(qual: Tree, qualTpe: Type, localTypeTags: Boolean = false): Option[(PartialSpec, Map[Symbol, Tree])] = {
      val pSpec = partialSpec.getOrElse(currentClass, Map.empty)
      val typeTags = typeTagTrees(currentMethod)
      qualTpe match {
        case ThisType(cls) => //if baseClass.isDefinedAt(cls) =>
          Some(pSpec, typeTags)
        case SingleType(pre, x) =>
          extractSpec(qual, qualTpe.widen)
        case TypeRef(pre, clazz, args) if specializedClasses.isDefinedAt(clazz) =>
          import miniboxing.runtime.MiniboxConstants._
          val adapter = (baseClass(clazz).typeParams zip clazz.typeParams).toMap.withDefault((x: Symbol) => x)
          Some((clazz.info.typeParams zip args).map({ case (tp, arg) =>
            val res = (tp, arg) match {
              // case (2.1)
              case (p, tpe) if pSpec.isDefinedAt(p) && typeTags.isDefinedAt(tpe.typeSymbol) =>
                ((p, pSpec(p)), (p, typeTags(tpe.typeSymbol)))
              // case (2.3)
              case (p, `UnitTpe`)    => ((p, Miniboxed), (p, Literal(Constant(UNIT))))
              case (p, `BooleanTpe`) => ((p, Miniboxed), (p, Literal(Constant(BOOLEAN))))
              case (p, `ByteTpe`)    => ((p, Miniboxed), (p, Literal(Constant(BYTE))))
              case (p, `ShortTpe`)   => ((p, Miniboxed), (p, Literal(Constant(SHORT))))
              case (p, `CharTpe`)    => ((p, Miniboxed), (p, Literal(Constant(CHAR))))
              case (p, `IntTpe`)     => ((p, Miniboxed), (p, Literal(Constant(INT))))
              case (p, `LongTpe`)    => ((p, Miniboxed), (p, Literal(Constant(LONG))))
              case (p, `FloatTpe`)   => ((p, Miniboxed), (p, Literal(Constant(FLOAT))))
              case (p, `DoubleTpe`)  => ((p, Miniboxed), (p, Literal(Constant(DOUBLE))))
              // case (2.2)
              // case (2.4)
              case (p, _) => ((p, Boxed), (p, Literal(Constant(REFERENCE))))
            }
//            println(res)
            res
          }).unzip match { case (ps, tt) => (ps.toMap, tt.toMap)} )
        case _ =>
//          println("None: " + showRaw(qualTpe))
          // unknown
          None
      }
    }

    def rewiredMethodCall(
          qual: Tree,
          oldMethodSym: Symbol,
          oldMethodType: Type,
          newMethodSym: Symbol,
          args: List[Tree],
          pspec: PartialSpec,
          tagTrees: Map[Symbol, Tree],
          tagTranslator: Function1[Symbol, Symbol] = (x: Symbol) => x): Tree = {
      // 1. Generate type tags
      val (tags, newMethodType) = separateTypeTagArgsInType(newMethodSym.tpe)
      val tagMapInv = localTypeTags(newMethodSym).map(_.swap).toMap
      val tagArgs = tags.map(tagMapInv andThen tagTranslator andThen tagTrees)
      val tagApp = gen.mkMethodCall(qual, newMethodSym, List(), tagArgs)
      //println(tagApp)

      // 2. Adapt arguments
      assert(newMethodType.paramss.length <= 1, "Cannot handle curried params. May be relaxed later.")
      val apply =
        if (args != null)
        {
          val newArgs =
            for((pForm, pAct) <- newMethodType.params zip args) yield {
              // pAct is always encoded using boxing
              // pForm may be encoded using either miniboxing OR boxing
              //println(pForm.tpe + " ==> " + pAct)
              if ((pForm.tpe == LongTpe) &&(pAct.tpe != LongTpe)) {
                gen.mkMethodCall(box2minibox, List(pAct.tpe), List(pAct, typeTagTrees(currentMethod)(pAct.tpe.typeSymbol)))
              }
              else
                pAct
            }
          val applyTree =
            if (newArgs.isEmpty)
              tagApp // created by the previous call to mkMethodCall
            else
              gen.mkMethodCall(tagApp, newArgs)
          //println(applyTree)
          applyTree
        } else
          tagApp

      // 3. Adapt return type
      //println("res: " + newMethodType.resultType + " ==> " + tree.tpe)
      val methodTypeTags = typeTagTrees(currentMethod)
      val unpackedTree =
        (newMethodType.resultType, oldMethodType.resultType) match {
          case (`LongTpe`, other) if other != LongTpe =>
            gen.mkMethodCall(minibox2box, List(other), List(apply, methodTypeTags(other.typeSymbol)))
          case _ =>
            apply
        }
      val unpacked = localTyper.typed(unpackedTree)
      //println(unpacked + ": " + unpacked.tpe + " (before: " + tree.tpe + ")")
      unpacked
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
          ltypedpos(gen.mkMethodCall(minibox2box, List(tpe), List(ttree, tagref)))
        case CastBoxToMinibox(tag) =>
          val tagref = localTyper.typed(gen.mkAttributedRef(tag))
          ltypedpos(gen.mkMethodCall(box2minibox, List(ttree.tpe), List(ttree, tagref)))
      }
    }

    /**
     * In `MiniboxInfoTransform` we create only symbols for methods.
     * Here we add empty bodies for them.
     */
    private def createMethodTrees(sClass: Symbol): List[Tree] = {
      val mbrs = new mutable.ListBuffer[Tree]
      for (m <- sClass.info.decls.toList.sortBy(_.defString) if m hasFlag MINIBOXED) {
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

              sClasses ++= specializedClasses(classSymbol).values.toList.sortBy(_.name.toString)

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
        body(member) = (rhs.duplicate, params)
        templateMembers -= member
        debug("collected " + member.fullName + ":")
        debug("  " + rhs)
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
    private def specializeDefDefBody(tree: DefDef, source: Symbol, castmap: TypeEnv = Map.empty) = {
      val meth = addDefDefBody(tree, source)

      duplicateBody(meth, source, castmap)
    }

    private def duplicateBody(tree: Tree, source: Symbol, castmap: TypeEnv = Map.empty) = {

      val symbol = tree.symbol
      val miniboxedSyms = miniboxedArgs.getOrElse(symbol, Nil)
      val miniboxedEnvDeep = typeEnv(symbol.owner).deepEnv
      val miniboxedEnvShallow = typeEnv(symbol.owner).shallowEnv
      val miniboxedTypeTags = typeTagTrees(symbol)

      debug(s"duplicating tree: for ${symbol} based on ${source}:\n${tree}")
      val currentReturn = symbol.tpe.finalResultType
      val originalReturn = source.tpe.finalResultType
      val miniboxedReturn = ((currentReturn =:= LongTpe) && !(originalReturn =:= LongTpe))
      debug(s"miniboxedReturn: current:${currentReturn} original:${originalReturn} miniboxedReturn:${miniboxedReturn}")
      if (miniboxedReturn) {
        assert(miniboxedEnvShallow(miniboxedEnvDeep(originalReturn.typeSymbol).typeSymbol) =:= LongTpe,
            s"Mismatching return type: current: ${currentReturn}, original: ${originalReturn}, typeEnv: ${miniboxedEnvShallow}")
      }

      val preparer = new MiniboxTreePreparer(unit, miniboxedSyms, miniboxedEnvDeep, miniboxedTypeTags, miniboxedReturn)
      val tree1 = preparer.transform(tree)

      val d = new Duplicator(castmap)
      debuglog("-->d DUPLICATING: " + tree1)
      val tree2 = d.retyped(
        localTyper.context1.asInstanceOf[d.Context],
        tree1,
        source.enclClass,
        symbol.enclClass,
        miniboxedEnvDeep
      )

      val specializer = new MiniboxTreeSpecializer(unit, Nil, miniboxedTypeTags, miniboxedEnvShallow)
      val tree3 = specializer.transform(tree2)

      tree3
    }

    /** Put the body of 'source' as the right hand side of the method 'tree'.
     *  The destination method gets fresh symbols for type and value parameters,
     *  and the body is updated to the new symbols, and owners adjusted accordingly.
     *  However, if the same source tree is used in more than one place, full re-typing
     *  is necessary. @see method duplicateBody
     */
    private def addDefDefBody(tree: DefDef, source: Symbol): DefDef = {
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
        false, false) // don't make private fields public

      val newBody = symSubstituter(body.duplicate)
      tpt.tpe = tpt.tpe.substSym(oldtparams, newtparams)

      copyDefDef(tree)(rhs = newBody)
    }

    private def addValDefBody(tree: Tree, origMember: Symbol): Tree = {
      val defSymbol = tree.symbol
      val origBody = MethodBodiesCollector.getFieldBody(origMember);
      val origClass = origMember.owner

      val tree1 = deriveValDef(tree)(_ => origBody.duplicate)
      debuglog("now typing: " + tree1 + " in " + tree.symbol.owner.fullName)

      duplicateBody(tree1, origMember)
    }
  }
}
