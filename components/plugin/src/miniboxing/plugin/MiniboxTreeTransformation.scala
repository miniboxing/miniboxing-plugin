package miniboxing.plugin

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.TypingTransformers
import scala.collection.mutable.Set
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.typechecker._

trait MiniboxTreeTransformation extends TypingTransformers {
  self: MiniboxComponent =>

  import global._
  import global.definitions._
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
    } with miniboxing.plugin.Duplicators {
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

    def typeTagTrees(member: Symbol = currentMethod, clazz: Symbol = currentClass) = {
      standardTypeTagTrees ++
      inheritedDeferredTypeTags.getOrElse(clazz, Map.empty).map({case (method, t) => (t, { gen.mkMethodCall(method, List())})}) ++
      primaryDeferredTypeTags.getOrElse(clazz, Map.empty).map({case (method, t) => (t, { gen.mkMethodCall(method, List())})}) ++
      globalTypeTags.getOrElse(clazz, Map.empty).map({case (t, tag) => (t, gen.mkAttributedSelect(gen.mkAttributedThis(tag.owner),tag))}) ++
      member.ownerChain.filter(_.isMethod).reverse.foldLeft(Map.empty[Symbol, Tree])((m, s) => m ++ localTypeTagTrees(s))
    }

    def localTypeTagTrees(symbol: Symbol): Map[Symbol, Tree] =
      localTypeTags.getOrElse(symbol, Map.empty).map({case (t, tag) => (t, Ident(tag))})

    import global._

    def afterMinibox[T](f: => T): T = atPhase(ownPhase.next)(f)

    override def transform(tree: Tree): Tree = miniboxTransform(tree)

    def miniboxTransform(tree: Tree): Tree = {
      curTree = tree
      // make sure specializations have been performed
      tree match {
        case t: SymTree if t.symbol != null => afterMinibox(t.symbol.info)
        case _ =>
      }

      tree match {
        /*
         *  We have created just the symbols for the specialized classes - now
         *  it's time to create their trees as well (initially empty).
         */
        case PackageDef(pid, classdefs) =>
          atOwner(tree, tree.symbol) {
            val specClassesTpls = createSpecializedClassesTrees(classdefs)
            val specClassesTped = specClassesTpls map localTyper.typed
            val templates = transformStats(classdefs ::: specClassesTped, tree.symbol.moduleClass)
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
          val specClassesTpls = createSpecializedClassesTrees(body)
          val specClassesTped = specClassesTpls map localTyper.typed
          val specMembers = createMethodTrees(tree.symbol.enclClass) map localTyper.typed
          val parents1 = map2(traitSym.info.parents, parents)((tpe, parent) => TypeTree(tpe) setPos parent.pos)
          val tree1 = localTyper.typedPos(tree.pos)(
            treeCopy.Template(tree, parents1, self,
              atOwner(currentOwner)(transformTrees(body.filter(defdef => traitDecls.contains(defdef.symbol)) ::: specMembers ::: specClassesTped))))
          tree1

        /*
         * The tree of a specialized class is empty for the moment, but we
         * create symbols for the methods - give them an empty body.
         */
        case Template(parents, self, body) =>
          val specMembers = createMethodTrees(tree.symbol.enclClass) map localTyper.typed
          val memberDefs = atOwner(currentOwner)(transformTrees(body ::: specMembers))
          val parents1 = map2(currentOwner.info.parents, parents)((tpe, parent) => TypeTree(tpe) setPos parent.pos)
          val templateDef = treeCopy.Template(tree, parents1, self, memberDefs)
          localTyper.typedPos(tree.pos)(templateDef)

        /*
         * The trait constructor -- which we leave empty as this is just a simple interface, nothing special about it
         */
        case ddef @ DefDef(mods, name, tparams, vparams :: Nil, tpt, _) if specializedBase(ddef.symbol.enclClass) && ddef.symbol.name != nme.MIXIN_CONSTRUCTOR && !notSpecializable(ddef.symbol.enclClass, ddef.symbol) =>
          localTyper.typed(treeCopy.DefDef(ddef, mods, name, tparams, vparamss = List(vparams), tpt, EmptyTree))

        /*
         * A definition with empty body - add a body as prescribed by the
         * `methodSpecializationInfo` data structure.
         */
        case ddef @ DefDef(mods, name, tparams, vparams :: Nil, tpt, EmptyTree) if hasInfo(ddef) =>
          val res = memberSpecializationInfo.apply(tree.symbol) match {
            // Implement the getter or setter functionality
            case FieldAccessor(field) =>
              val localTypeArgs = localTypeTags(tree.symbol)
              val allArgs = tree.symbol.tpe.params
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
              val (ttagWrapperArgs, params) = separateTypeTagArgsInTree(vparams)
              val (ttagFormalArgs, targetParams) = separateTypeTagArgsInType(target.tpe)

              assert(params.length == targetParams.length, "Different number of parameters for forward from " + tree.symbol.defString + " to " + target.defString + ": " + params + " vs " + targetParams)

              val params1 =
                ((params zip targetParams) zip paramCasts) map {
                  case ((p, t), paramCast) =>
                    cast(Ident(p.symbol), t.tpe, paramCast)
                }
              val rhs1 = gen.mkMethodCall(target, ttagArgs.map(gen.mkAttributedRef(_)) ::: params1)
              super.transform(localTyper.typed(deriveDefDef(tree)(_ => cast(rhs1, tpt.tpe, retCast))))

            // copy the body of the `original` method
            case SpecializedImplementationOf(target) =>
              // we have an rhs, specialize it
              def reportTypeError(body: =>Tree) = reportError(body)(_ => ddef)
              val tree1 = specializeDefDefBody(ddef, target)
              debuglog("implementation: " + tree1)
              tree1

            case _: Interface | _ : DeferredTypeTag =>
              tree

            case DeferredTypeTagImplementation(tparam) =>
              val tagTrees = typeTagTrees()
              val localTParam = tparam.tpe.asSeenFrom(currentClass.info.prefix, currentClass).typeSymbol
              super.transform(localTyper.typed(deriveDefDef(tree)(_ => localTyper.typed(tagTrees(localTParam)))))

            case info =>
              super.transform(localTyper.typed(treeCopy.DefDef(tree, mods, name, tparams, List(vparams), tpt, localTyper.typed(Block(Ident(Predef_???))))))
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

        // ???
        case DefDef(mods, name, tparams, vparamss, tpt, body) if (tree.symbol.isConstructor &&
          tree.symbol.paramss.head.size != vparamss.head.size) =>
          debug(" => overriding constructor in " + tree.symbol.ownerChain.reverse.map(_.nameString).mkString(".") + ":\n" + tree)
          val result = localTyper.typedPos(tree.pos)(DefDef(tree.symbol, _ => body))
          debug(" <= " + result)
          super.transform(result)

        // Redirect method calls without type parameters
        // TODO: unify with the next case
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
                extractSpec(qual.tpe, currentMethod, currentClass) match { // Get the partial specialization
                  case Some(pspec) if !isAllAnyRef(pspec) && overloads.get(oldMethodSym).flatMap(_.get(pspec)).isDefined =>
                    val newMethodSym = overloads(oldMethodSym)(pspec)
                    val tree1 = rewiredMethodCall(qual, oldMethodSym, oldMethodType, newMethodSym, currentClass.info.memberInfo(newMethodSym), args, null)
                    stats("redirecting method call: " + tree + " ==> " + tree1)
                    tree1
                  case other =>
                    tree
                }
            }
          super.transform(tree1)

        // Redirect method calls with type parameters
        case Apply(TypeApply(sel @ Select(qual, fn), targs), args) if { afterMinibox(sel.symbol.owner.info); base.isDefinedAt(tree.symbol) && base(tree.symbol) == tree.symbol } =>
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
                extractSpec(qual.tpe, currentMethod, currentClass) match { // Get the partial specialization
                  case Some(pspec) if !isAllAnyRef(pspec) && overloads.get(oldMethodSym).flatMap(_.get(pspec)).isDefined =>
                    val newMethodSym = overloads(oldMethodSym)(pspec)
                    val tree1 = rewiredMethodCall(qual, oldMethodSym, oldMethodType, newMethodSym, currentClass.info.memberInfo(newMethodSym), args, targs)
                    stats("redirecting method call: " + tree + " ==> " + tree1)
                    tree1
                  case other =>
                    tree
                }
            }
          super.transform(tree1)

        // Redirect field selections
        case Select(qual, fn) if { afterMinibox(tree.symbol.owner.info); base.isDefinedAt(tree.symbol) && base(tree.symbol) == tree.symbol && !tree.symbol.isMethod } =>
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
              extractSpec(qual.tpe, currentMethod, currentClass) match { // Get the partial specialization
                  case Some(pspec) if !isAllAnyRef(pspec) && overloads.get(oldMethodSym).flatMap(_.get(pspec)).isDefined =>
                    val newMethodSym = overloads(oldMethodSym)(pspec)
                    val tree1 = rewiredMethodCall(qual, oldMethodSym, oldMethodType, newMethodSym, currentClass.info.memberInfo(newMethodSym), null, null)
                    stats("redirected selection: " + tree + " ==> " + tree1)
                    tree1
                  case other =>
                    tree
                }
            }
          super.transform(tree1)

        // Super constructor call rewiring for:
        //  - non-specialized classes
        //  - specialized classes
        case Apply(sel @ Select(sup @ Super(ths, name), nme.CONSTRUCTOR), args)
          if {
            afterMinibox(sup.tpe.typeSymbol.info)
            // either the parents changed, or class is specialized (specialized classes
            // don't exist before minibox, thus their parents don't *change* but *appear*)
            (sup.symbol.info.parents != beforeMinibox(sup.symbol.info.parents)) || baseClass.isDefinedAt(sup.symbol) && (baseClass(sup.symbol) != sup.symbol) &&
            // don't rewire super constructor if the parent is AnyRef
            // TODO: Shouldn't this be done for any non-specialized class?
            !(sup.symbol.info.parents.head =:= AnyRefTpe)
          } =>
            val oldInitSym = sel.symbol
            val oldInitTpe = sel.tpe // do we want the instantiated type?
            // Someday I will rewrite that damn typer! I spend one entire day to find out that a
            // Select(Super(_, _), _) is typed differently from a Super(_, _)...
            val init = localTyper.typedOperator(Select(Super(ths, name) setPos sup.pos, nme.CONSTRUCTOR))
            val newInitSym = init.symbol
            val newInitTpe = currentClass.info.memberInfo(newInitSym)
            if (newInitSym == oldInitSym)
              tree
            else {
              val Select(qual, _) = init
              val tree1 = rewiredMethodCall(qual, oldInitSym, oldInitTpe, newInitSym, newInitTpe, args.map(transform), null)
              localTyper.typed(tree1)
            }

        // Constructor call rewiring for specialized classes
        //   new C[Int](3) => new C_J[Int](INT)(3.toInt)
        case Apply(ctor @ Select(qual @ New(cl), nme.CONSTRUCTOR), args) if { afterMinibox(cl.symbol.info); specializedClasses.isDefinedAt(qual.tpe.typeSymbol) } =>
          val oldClassCtor = ctor.symbol
          val tree1 = cl.tpe match {
            case TypeRef(pre, oldClass, targs) =>
              extractSpec(cl.tpe, currentMethod, currentClass) match {
                case Some(pspec) if !isAllAnyRef(pspec) =>
                  assert(specializedClasses(oldClass).isDefinedAt(pspec))
                  assert(overloads.isDefinedAt(ctor.symbol))
                  assert(overloads(ctor.symbol).isDefinedAt(pspec))
                  val newClass = specializedClasses(oldClass)(pspec)
                  val newClassCtor = overloads(oldClassCtor)(pspec)
                  val newQual = localTyper.typedOperator(New(TypeTree(TypeRef(pre, newClass, targs))))
                  val tree1 = rewiredMethodCall(newQual, oldClassCtor, ctor.tpe, newClassCtor, currentClass.info.memberInfo(newClassCtor), args.map(transform), null)
                  stats("redirecting new: " + tree + " ==> " + tree1)
                  tree1
                case Some(_) =>
                  val allAnyRefSpec = oldClass.typeParams.filter(_ hasFlag MINIBOXED).map(t => (t, Boxed)).toMap
                  val newClass = specializedClasses(oldClass)(allAnyRefSpec)
                  val newClassCtor = overloads(oldClassCtor)(allAnyRefSpec)
                  val newQual = localTyper.typedOperator(New(TypeTree(TypeRef(pre, newClass, targs))))
                  gen.mkMethodCall(newQual, newClassCtor, List(), args.map(transform))
                case None =>
                  global.reporter.error(tree.pos, "Unable to rewire constructor, this will probably lead to invalid bytecode.")
                  tree
              }
            case _ =>
              global.reporter.error(tree.pos, "Unsupported new operation.")
              tree
          }
          localTyper.typed(tree1)

        // Error on accessing non-existing fields
        case sel@Select(ths, field) if (ths.symbol ne null) && (ths.symbol != NoSymbol) && { afterMinibox(ths.symbol.info); specializedBase(ths.symbol) && (sel.symbol.isValue && !sel.symbol.isMethod) } =>
          unit.error(sel.pos, "The program is accessing field " + sel.symbol.name + " of miniboxed class (or trait) " + ths.symbol.name + ", a pattern which becomes invalid after the miniboxing transformation. Please allow Scala to generate getters (and possibly setters) by using val (or var) without the \"private[this]\" qualifier: " + (if (sel.symbol.isMutable) "var " else "val ") + sel.symbol.name + ": " + sel.symbol.info + "\".")
          localTyper.typed(gen.mkAttributedRef(Predef_???))

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
              super.transform(tree)
          }
          localTyper.typed(tree1)

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
              super.transform(tree)
          }
          localTyper.typed(tree1)

        // Array new
        case Apply(newArray @ Select(manifest, _), List(size)) if newArray.symbol == Manifest_newArray =>
          val tags = typeTagTrees()
          val tree1 = manifest.tpe.widen.typeArgs match {
            case tpe :: Nil if tags.isDefinedAt(tpe.typeSymbol) =>
              val tag = tags(tpe.typeSymbol)
              val tree1 = gen.mkMethodCall(mbarray_new, List(tpe), List(size, tag))
              //val tree2 = gen.mkAsInstanceOf(tree1, tree.tpe)
              stats("rewrote array new: " + tree + " ==> " + tree1)
              tree1
            case _ =>
              super.transform(tree)
          }
          localTyper.typed(tree1)

        // Array length
        case Apply(length @ Select(array, _), Nil) if length.symbol == Array_length =>
          val tags = typeTagTrees()
          val tree1 = array.tpe.widen.typeArgs match {
            case tpe :: Nil if tags.isDefinedAt(tpe.typeSymbol) =>
              val tag = tags(tpe.typeSymbol)
              val tree1 = gen.mkMethodCall(mbarray_length, List(array, tag))
              stats("rewrote array length: " + tree + " ==> " + tree1)
              tree1
            case _ =>
              super.transform(tree)
          }
          localTyper.typed(tree1)

        case _ =>
          super.transform(tree)
      }
    }

    def extractSpec(qualTpe: Type, inMethod: Symbol, inClass: Symbol): Option[PartialSpec] = {
      val pSpec = partialSpec.getOrElse(inClass, Map.empty)
      qualTpe match {
        case ThisType(cls) if (cls == inClass) =>
          Some(pSpec)
        case SingleType(pre, x) =>
          extractSpec(qualTpe.widen, inMethod, inClass)
        case PolyType(tparams, rest) =>
          extractSpec(rest, inMethod, inClass)
        case TypeRef(pre, clazz, args) if specializedClasses.isDefinedAt(clazz) =>
          import miniboxing.runtime.MiniboxConstants._
          val tparams = afterMinibox(clazz.info).typeParams
          val spec = (tparams zip args) flatMap { (pair: (Symbol, Type)) =>
            pair match {
              // case (2.3)
              case (p, _) if !(p hasFlag MINIBOXED) => None
              case (p, `UnitTpe`)    => Some((p, Miniboxed))
              case (p, `BooleanTpe`) => Some((p, Miniboxed))
              case (p, `ByteTpe`)    => Some((p, Miniboxed))
              case (p, `ShortTpe`)   => Some((p, Miniboxed))
              case (p, `CharTpe`)    => Some((p, Miniboxed))
              case (p, `IntTpe`)     => Some((p, Miniboxed))
              case (p, `LongTpe`)    => Some((p, Miniboxed))
              case (p, `FloatTpe`)   => Some((p, Miniboxed))
              case (p, `DoubleTpe`)  => Some((p, Miniboxed))
              // case (2.1)
              // case (2.2)
              // case (2.4)
              case (p, tpe) if baseClass.isDefinedAt(inClass) =>
                val instantiatedBaseClass = inClass.info.baseType(baseClass(inClass))
                val instantiationMapper = (instantiatedBaseClass.typeArgs.map(_.typeSymbol).zip(instantiatedBaseClass.typeSymbol.typeParams)).toMap
                val baseTParam = instantiationMapper.getOrElse(tpe.typeSymbol, NoSymbol)
                if (pSpec.isDefinedAt(baseTParam))
                  Some((p, pSpec(baseTParam)))
                else
                  Some((p, Boxed))
              case (p, _) =>
                  Some((p, Boxed))
            }
          }
          Some(spec.toMap)
        case _ =>
          // unknown
          None
      }
    }

    def rewiredMethodCall(
          newQual: Tree,
          oldMethodSym: Symbol,
          oldMethodTpe: Type,
          newMethodSym: Symbol,
          newMethodTpe: Type,
          args: List[Tree],
          targs: List[Tree]) = {

      // 1. Generate type tags
      val (tagSyms, argTypes) = separateTypeTagArgsInType(newMethodSym.info)
      // Tag -> Type param
      val tagsToTparams1 = localTypeTags(newMethodSym).map(_.swap).toMap
      val tparamInsts = for (tagSym <- tagSyms) yield {
        try {
          val tparam = tagsToTparams1(tagSym)
          val instOwner = newQual.tpe.baseType(tparam.owner)
          val tparamToInst = (instOwner.typeSymbol.typeParams zip instOwner.typeArgs).toMap
          tparamToInst(tparam).typeSymbol
        } catch {
          case ex: Exception =>
            println("Tag not found:")
            println(newMethodSym)
            println(tagSyms)
            println(argTypes)
            println(tagsToTparams1)
            println(currentClass)
            println(currentMethod)
            println(typeTagTrees())
            ex.printStackTrace()
            System.exit(1)
            ???
        }
      }
      val typeTags = typeTagTrees()
      val localTagArgs = tparamInsts.map(typeTags)


      // 2. Adapt arguments
      val adaptedArgs =
        if (args != null)
          for((pForm, pAct) <- (argTypes zip args)) yield {
            // pAct is always encoded using boxing
            // pForm may be encoded using either miniboxing OR boxing
            if ((pForm.tpe == LongTpe) &&(pAct.tpe != LongTpe))
              gen.mkMethodCall(box2minibox, List(pAct.tpe), List(pAct, typeTags(pAct.tpe.typeSymbol)))
            else
              pAct
          }
        else
          Nil

      // TODO: Implement type parameter translation
      val method = gen.mkAttributedSelect(newQual, newMethodSym)
      val tapp = if (targs == null) method else gen.mkTypeApply(method, targs)
      val aapp = gen.mkMethodCall(tapp, localTagArgs ::: adaptedArgs)

      // 3. Adapt return type
      val unpackedTree =
        (newMethodTpe.finalResultType, oldMethodTpe.finalResultType) match {
          case (`LongTpe`, other) if other != LongTpe =>
            gen.mkMethodCall(minibox2box, List(other), List(aapp, typeTags(other.typeSymbol)))
          case _ =>
            aapp
        }
      val unpacked = localTyper.typed(unpackedTree)

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
      val tree0 = ltypedpos(tree)
      val tree1 = cinfo match {
        case NoCast =>
          tree0
        case AsInstanceOfCast =>
          gen.mkAsInstanceOf(tree0, tpe, true, false)
        case CastMiniboxToBox(tag) =>
          val tagref = localTyper.typed(gen.mkAttributedRef(tag))
          gen.mkMethodCall(minibox2box, List(tpe), List(tree0, tagref))
        case CastBoxToMinibox(tag) =>
          val tagref = localTyper.typed(gen.mkAttributedRef(tag))
          gen.mkMethodCall(box2minibox, List(tree0.tpe), List(tree0, tagref))
      }
      val tree2 = ltypedpos(tree1)
      tree2
    }

    /**
     * In `MiniboxInfoTransform` we create only symbols for methods.
     * Here we add empty bodies for them.
     */
    private def createMethodTrees(sClass: Symbol): List[Tree] = {
      val mbrs = new ListBuffer[Tree]
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
      val buf = new ListBuffer[Tree]
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
      private val body = HashMap[Symbol, (Tree, List[List[Symbol]])]()

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
      val miniboxedSyms = miniboxedArgs.getOrElse(symbol, Set())
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

      val preparer = new MiniboxTreePreparer(unit, source.enclClass, currentClass, miniboxedSyms, miniboxedEnvDeep, miniboxedEnvShallow, miniboxedTypeTags, miniboxedReturn)
      val tree1 = preparer.transform(tree)

      val d = new Duplicator(castmap)
      debuglog("-->d DUPLICATING: " + tree1)

      // Duplicator chokes on retyping new C if C is marked as abstract
      // but we need this in the backend, else we're generating invalid
      // flags for the entire class - for better or worse we adapt just
      // before calling the duplicator, and get back for specialization
      for (clazz <- specializedBase)
        clazz.resetFlag(ABSTRACT)

      val tree2 = beforeMinibox(d.retyped(
        localTyper.context1.asInstanceOf[d.Context],
        tree1,
        source.enclClass,
        symbol.enclClass,
        miniboxedEnvDeep
      ))

      // get back flags
      for (clazz <- specializedBase)
        clazz.setFlag(ABSTRACT)

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
      val (tparams, tags, vparams, tpt) = tree match {
        case DefDef(_, _, tparams, tvparams :: Nil, tpt, _) if base.getOrElse(symbol, NoSymbol) != symbol =>
          val (ttags, vparams) = separateTypeTagArgsInTree(tvparams)
          (tparams, ttags, vparams, tpt)
        case DefDef(_, _, tparams, vparams :: Nil, tpt, _) if base.getOrElse(symbol, NoSymbol) == symbol =>
          (tparams, Nil, vparams, tpt)
      }
      val env = typeEnv(symbol.owner).deepEnv // TODO
      val boundTvars = env.keySet
      val origtparams = source.typeParams.filter(tparam => !boundTvars(tparam) || !isPrimitiveValueType(env(tparam)))
      if (origtparams.nonEmpty || symbol.typeParams.nonEmpty)
        debuglog("substituting " + origtparams + " for " + symbol.typeParams)

      // skolemize type parameters - not really needed, duplicator will do the job
      val oldtparams = tparams map (_.symbol)
      val newtparams = oldtparams//deriveFreshSkolems(oldtparams)
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
        vparams.map(_.symbol) ::: newtparams,
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
