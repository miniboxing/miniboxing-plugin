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
//    * Cristian Talau
//
package miniboxing.plugin
package transform
package minibox
package inject

import scala.tools.nsc.transform.TypingTransformers
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.typechecker._
import scala.reflect.internal.Flags._

trait MiniboxInjectTreeTransformation extends TypingTransformers {
  self: MiniboxInjectComponent =>

  import global._
  import global.definitions._
  import typer.{ typed, atOwner }
  import memberSpecializationInfo._

  var ttindent = 0

  // TODO: This should be:
  // 1) lazy in computing the trees
  // 2) moved over to miniboxing logic, as it's not specific to trees
  def typeTagTrees(owner: Symbol): Map[Symbol, Tree] = {
    ttindent += 1
    val prev = if (owner.owner != NoSymbol) typeTagTrees(owner.owner) else Map.empty
    val res = prev ++
      metadata.inheritedDeferredTypeTags.getOrElse(owner, Map.empty).map({case (method, t) => (t, { gen.mkMethodCall(method, List())})}) ++
      metadata.primaryDeferredTypeTags.getOrElse(owner, Map.empty).map({case (method, t) => (t, { gen.mkMethodCall(method, List())})}) ++
      metadata.globalTypeTags.getOrElse(owner, Map.empty).map({case (tag, t) => (t, gen.mkAttributedSelect(gen.mkAttributedThis(tag.owner),tag))}) ++
      metadata.localTypeTags.getOrElse(owner, Map.empty).map({case (tag, t) => (t, gen.mkAttributedIdent(tag))}) ++
      metadata.normalTypeTags.getOrElse(owner, Map.empty).map({case (tag, t) => (t, gen.mkAttributedIdent(tag))}) ++
      standardTypeTagTrees // override existing type tags
    ttindent -= 1
    res
  }

  def reportError[T](location: Symbol)(body: =>T)(handler: TypeError => T): T =
    try body
    catch {
      case te: TypeError =>
        if (location != NoSymbol)
          reporter.error(te.pos, s"[ occured while creating miniboxed ${location.fullLocationString} ]\n${te.msg}")
        handler(te)
//        throw(te)
    }

  /**
   * The tree transformer that adds the trees for the specialized classes inside
   * the current package.
   */
  class MiniboxTreeTransformer(unit: CompilationUnit) extends TreeRewriter(unit) {

    import global._

    val bug64message =
      if (!flag_constructor_spec)
        "Please note this is caused by the fact that the miniboxing plugin was instructed not to specialize side-effecting constructor statements."
      else
        ""

    /** This duplicator additionally performs casts of expressions if that is allowed by the `casts` map. */
    class Duplicator(casts: Map[Symbol, Type], oldThis: Symbol, newThis: Symbol, _subst: TypeMap, _deepSubst: TypeMap) extends {
      val global: MiniboxInjectTreeTransformation.this.global.type = MiniboxInjectTreeTransformation.this.global
      val miniboxing: MiniboxInjectComponent { val global: MiniboxInjectTreeTransformation.this.global.type } =
        MiniboxInjectTreeTransformation.this.asInstanceOf[MiniboxInjectComponent { val global: MiniboxInjectTreeTransformation.this.global.type }]
    } with miniboxing.plugin.transform.minibox.inject.Duplicators {

      def flag_create_local_specs = MiniboxInjectTreeTransformation.this.flag_create_local_specs
      override def postTransform(owner: Symbol, tree: Tree): Tree = {
        //val tree1 = new BridgeTransformer(unit).addBridges(owner, tree)
        tree
      }
      def suboptimalCodeWarning(pos: Position, msg: String, isSymbolGenericAnnotated: Boolean = false) = MiniboxInjectTreeTransformation.this.suboptimalCodeWarning(pos, msg, isSymbolGenericAnnotated)

      // utility method:
      def retyped(context0: MiniboxInjectTreeTransformation.this.global.analyzer.Context, tree0: Tree) = {

        // Duplicator chokes on retyping new C if C is marked as abstract
        // but we need this in the backend, else we're generating invalid
        // flags for the entire class - for better or worse we adapt just
        // before calling the duplicator, and get back for specialization
        preMiniboxingFlags()

        val res =
          util.Try {
            beforeMiniboxInject {
              super.retyped(context0.asInstanceOf[Context],
                            tree0,
                            oldThis,
                            newThis,
                            _subst,
                            _deepSubst)
            }
          }

        // get back flags
        postMiniboxingFlags()

        res.get
      }
    }

    class RemovedFieldFinder(stemClass: Symbol) extends Traverser {
      val removedMbrs = flagdata.stemClassRemovedMembers.getOrElse(stemClass, collection.mutable.Set.empty[Symbol])
      def skipLocalDummy(sym: Symbol): Symbol =
        if (sym.isLocalDummy) skipLocalDummy(sym.owner) else sym

      override def traverse(tree: Tree): Unit = {
        if (tree.hasSymbolField && removedMbrs(tree.symbol)) {
          val mbr = tree.symbol

          val additionalMsg =
            if (skipLocalDummy(currentOwner) == stemClass) bug64message else ""

          if (mbr.isField) {
            val suggested = s""""${(if (mbr.isMutable) "var " else "val ") + mbr.name + ": " + mbr.tpe}""""
            val message =
              if (mbr.isParamAccessor)
                "adding val to the constructor parameter: "
              else if (mbr.isPrivateThis)
                "removing the \"private[this]\" qualifier: "
              else
                "making it public: "

            global.reporter.error(tree.pos, "The following code is accessing field " + mbr.name + " of miniboxed " +
                                            stemClass.tweakedToString + ", a pattern which becomes invalid after the " +
                                            "miniboxing transformation. Please allow Scala to generate accessors " +
                                            "for this field by " + message + suggested + ". " + additionalMsg)
          } else if (!mbr.isConstructor) {
            global.reporter.error(tree.pos, "You ran into a fundamental limitation of the miniboxing transformation. " +
                                  "When miniboxing " + stemClass.tweakedToString + ", the miniboxing plugin moves " +
                                  "all the fields and super-accessors to the specialized subclasses. Therefore, trying " +
                                  "to access them in the nested " + skipLocalDummy(currentOwner).tweakedFullString +
                                  " is not a valid pattern anymore. Please read " +
                                  "https://github.com/miniboxing/miniboxing-plugin/issues/166 " +
                                  "for a thorough explanation and some workarounds for the problem. " +
                                  additionalMsg + "Thanks and sorry! " + additionalMsg)
          } else { // mbr.isConstructor
            // do nothing, it's going to get rewired to a specialized class
          }
        }
        super.traverse(tree)
      }
      def find(owner: Symbol, tree: Tree) =
        atOwner(owner){ traverse(tree) }
    }

    def typeTagTrees(member: Symbol = currentOwner) =
      MiniboxInjectTreeTransformation.this.typeTagTrees(member)

    def miniboxQualifier(pos: Position, tpe: Type): Type = {
      val oldClass = tpe.typeSymbol
      val newClass =
        extractSpec(pos, tpe, currentOwner) match {
          case _ if !metadata.isClassStem(oldClass)=>
            oldClass
          case Some(pspec) =>
            metadata.classOverloads(oldClass)(pspec)
          case None =>
            global.reporter.error(pos, "Unable to rewire term, this will probably lead to invalid bytecode.")
            oldClass
        }

      tpe match {
        case TypeRef(pre, _, targs) =>
          typeRef(pre, newClass, targs)
        case ThisType(_) =>
          ThisType(newClass)
      }
    }

    def extractQualifierType(tree: Tree): Type = tree match {
      case New(cl)     => afterMiniboxInject(cl.tpe.typeSymbol.info); cl.tpe
      case This(clazz) => afterMiniboxInject(tree.symbol.info); tree.tpe.underlying
      case Super(qual, _) => tree.tpe
      case _ => if (tree.hasSymbolField && tree.symbol != null) afterMiniboxInject(tree.symbol.info.typeSymbol.info); tree.tpe
    }

    def extractFunctionQualifierType(tree: Tree): Type = tree match {
      case Select(newQual, _) => newQual.tpe
      case TypeApply(fun, _) => extractFunctionQualifierType(fun)
      case Apply(fun, _) => extractFunctionQualifierType(fun)
      case _ => NoType
    }

    /** Goes from a member in the stem class to its equivalent in the specialized class variant.
     * ```
     * class C[@miniboxed T] {
     *   def foo = ???
     * }
     * ```
     *  =>
     * ```
     * trait C[T] {
     *   def foo: Nothing // (1)
     * }
     * class C_J[T](...) extends C[T] {
     *   def foo: Nothing = ??? // (2)
     * }
     *
     * Namely from (1) to (2), where sym points to `C`'s `foo` and specClass points to `C_J`
     */
    def equivalentMemberInSpecializedClass(sym: Symbol, specClass: Symbol): Option[Symbol] = {
      assert(metadata.isClassStem(sym.owner), sym.owner)
      assert(!metadata.isClassStem(specClass), specClass)
      val specSym =
        if (sym.isConstructor) {
          val pspec = metadata.classSpecialization(specClass)
          val res = metadata.memberOverloads.get(sym).flatMap(_.get(pspec)).getOrElse(NoSymbol)
          res
        } else if (!sym.isMethod && sym.isValue) {
          specClass.tpe.decl(sym.name)
        }else {
          var member = specClass.tpe.decl(sym.name)
          if (member.isOverloaded)
            member = member.filter(_.allOverriddenSymbols contains sym)
          member
        }

      assert(!specSym.isOverloaded, s"[miniboxing internal error] Cannot find equivalent symbol of $sym (owner=${sym.owner}) in $specClass: ${specSym.defString}")
      if (specSym == NoSymbol)
        None
      else
        Some(specSym)
    }

    def rewrite(tree: Tree): Result = {
      curTree = tree

      def sym = if (tree.hasSymbolField) tree.symbol else NoSymbol

      tree match {
        case Select(qual, _) =>
          qual.tpe.typeSymbol.info
        case _ if tree.hasSymbolField && tree.symbol != null =>
          tree.symbol.info
        case _ =>
      }

      tree match {

        // Miniboxing reflection
        case MiniboxingReflectionMethod(rewrite_fn) =>
          localTyper.typed(rewrite_fn(currentOwner))

        case ClassDef(_, _, _, impl: Template) =>

          if (heuristics.isSpecializableClass(tree.symbol) && metadata.isClassStem(tree.symbol)) {
            // The base trait for the current class
            val specializedStemClassSym = tree.symbol
            val baseTrait = deriveClassDef(tree)(rhs => atOwner(tree.symbol)(transformTemplate(rhs)))

            // The specialized classes for the current class
            val specClassSymbols = metadata.classOverloads(specializedStemClassSym).values.toList.sortBy(_.name.toString)
            val specClasses: List[Tree] =
              for (specClassSym <- specClassSymbols) yield {
                debuglog("Creating specialized class " + specClassSym.defString + " for " + specializedStemClassSym)
                val classDef = atPos(specializedStemClassSym.pos)(classDefTreeFromSym(specClassSym))
                val classDef2 =
                  deriveClassDef(classDef)(templ =>
                    deriveTemplate(templ)(_ =>
                      impl.body.filter(x => !x.hasSymbolField || !x.symbol.isMixinConstructor).map(_.duplicate)
                    ))
                // type check and transform the class before returning the tree
                transform(localTyper.typed(classDef2))
              }

            baseTrait :: specClasses
          } else {
            Descend
          }

        case Template(parents, self, body) =>
          afterMiniboxInject(tree.symbol.enclClass.info)
          MethodBodiesCollector(tree)

          //  This is either a class that has nothing to do with miniboxing or that is the base
          //  class (now trait) for the specialization.
          //  Also collect the bodies of the methods that need to be copied and specialized.
          val cls = tree.symbol.enclClass
          val decls = afterMiniboxInject(cls.info).decls.toList
          val tags = decls.filter(memberSpecializationInfo.get(_).map(_.isTag).getOrElse(false)).sortBy(_.nameString)
          def memberVariants(mbr: Symbol): List[Tree] = {
            val specVariants = metadata.memberOverloads.get(mbr).map(_.values.toList.sortBy(_.nameString)).getOrElse(List(mbr))
            specVariants.filter(_ != mbr).flatMap(mbr => createMemberTree(Some(mbr)))
          }
          val state = metadata.getClassState(cls)
          def isConstructorOrField(sym: Symbol) = (
            sym.isConstructor ||
            sym.isMixinConstructor ||
            (sym.isValue && !sym.isMethod))

          val bodyDefs =
            state match {

              case NotSpecialized =>
                body.flatMap({
                  case dd: DefDef =>
                    dd :: memberVariants(dd.symbol)
                  case other =>
                    List(other)
                })

              case SpecializedStem =>
                var sideEffectWarning = true
                val removedFieldFinder = new RemovedFieldFinder(cls)
                body.flatMap({
                  case dt: DefTree if isConstructorOrField(dt.symbol) || dt.symbol.isSuperAccessor =>
                    Nil
                  case dd: DefDef if heuristics.specializableMethodInClass(cls, dd.symbol) =>
                    if (!dd.symbol.isPrivate)
                      deriveDefDef(dd)(_ => EmptyTree) :: memberVariants(dd.symbol)
                    else {
                      if (!flag_constructor_spec)
                        global.reporter.error(dd.pos, "The " + dd.symbol + " should be made public or the constructor " +
                                                      "specialization should be enabled, otherwise the translation " +
                                                      "of this method will be incorrect. Plase see " +
                                                      "https://github.com/miniboxing/miniboxing-plugin/issues/86 " +
                                                      "for more details." )
                      Nil
                    }
                  case cd: ClassDef =>
                    suboptimalCodeWarning(cd.pos, "The " + cd.symbol.tweakedToString + " will not be miniboxed based " +
                                                  "on type parameter(s) " + cls.typeParams.map(_.nameString).mkString(", ") +
                                                  " of miniboxed " + cls.tweakedToString + ". To have it specialized, " +
                                                  "add the type parameters of " + cls.tweakedToString + ", marked with " +
                                                  "\"@miniboxed\" to the definition of " + cd.symbol.tweakedToString +
                                                  " and instantiate it explicitly passing the type parameters from " +
                                                  cls.tweakedToString + ":", cd.symbol.isGenericAnnotated)
                    removedFieldFinder.find(currentOwner, cd)
                    List(cd)
                  case dt: DefTree =>
                    removedFieldFinder.find(currentOwner, dt)
                    List(dt)
                  case other =>
                    if (!flag_constructor_spec) {
                      if (sideEffectWarning) {
                        global.reporter.warning(other.pos,
                            s"The side-effecting statement(s) in the miniboxed ${cls.tweakedToString}'s constructor " +
                            "will not be miniboxed. " + bug64message)
                        sideEffectWarning = false
                      }
                      removedFieldFinder.find(currentOwner, other)
                      List(other)
                    }  else
                      Nil
                })

              case SpecializedVariant =>
                val stemClass = metadata.getClassStem(cls)
                val duplicator = prepareDuplicator(None, None, cls, stemClass)

                def equivalentMemberTree(sym: Symbol): Option[Tree] = createMemberTree(equivalentMemberInSpecializedClass(sym, cls))

                body.flatMap({
                  case dt: DefTree if isConstructorOrField(dt.symbol) =>
                    equivalentMemberTree(dt.symbol).toList
                  case dd: DefDef if !dd.symbol.isSuperAccessor => // superaccessors are added by mixin
                    val equiv = equivalentMemberTree(dd.symbol)
                    equiv.map(dd => dd :: memberVariants(dd.symbol)).getOrElse(Nil)
                  case _: DefTree =>
                    Nil
                  case other =>
                    if (flag_constructor_spec)
                      reportError(NoSymbol){
                        val context = atOwner(tree.symbol)(localTyper.context1)
                        // we need to wrap this in a block, in order to force invalidation:
                        val other2 =
                          duplicator.retyped(context, Block(Nil, other)) match {
                            case Block(_, res) => res
                          }
                        List(other2)
                      } {
                        (t: TypeError) =>
                          global.reporter.error(t.pos, "An error was encountered while specializing constructor code for " +
                                                       stemClass.tweakedToString + ". Please please please report this " +
                                                       "error to https://github.com/miniboxing/miniboxing-plugin/issues! " +
                                                       "As a workaround, you can add the following scala compiler flag: " +
                                                       "\"-P:minibox:Ygeneric-constructor-code\". The performance will " +
                                                       "degrade as a result and you'll see a lot of warnings, but the " +
                                                       "program should compile. (internal error: " + t.msg + ")")
                          t.printStackTrace()
                          List(localTyper.typed(gen.mkMethodCall(Predef_???, Nil)))
                      }
                    else
                      Nil
                })
            }

          // members
          val tagMembers = tags.flatMap(tag => createMemberTree(Some(tag)))
          val memberDefs = atOwner(currentOwner)(transformStats(tagMembers ::: bodyDefs, cls))

          // parents
          val parents1 = map2(cls.info.parents, parents)((tpe, parent) => TypeTree(tpe) setPos parent.pos)

          // new template def
          val templateDef = treeCopy.Template(tree, parents1, self, atOwner(currentOwner)(memberDefs))
          val tree1 = localTyper.typedPos(tree.pos)(templateDef)

          tree1

        // A definition
        case ddef @ DefDef(mods, name, tparams, vparams :: Nil, tpt, body) =>

          val res: Tree =
            memberSpecializationInfo.get(sym) match {
              // Implement the getter or setter functionality
              case Some(FieldAccessor(field)) =>
                val localTypeArgs = metadata.localTypeTags(tree.symbol)
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
              case Some(ForwardTo(target)) =>
                val (ttagWrapperArgs, wrapperParams) = tagUtils.separateTypeTagArgsInTree(vparams)
                debuglog("")
                debuglog(s"Forwarding to $target for $tree")
                val rhs1 = gen.mkMethodCall(target, tparams.map(_.symbol.tpeHK), wrapperParams.map(param => Ident(param.symbol)))
                val rhs2 = localTyper.typed(rhs1)
                val rhs3 = atOwner(tree.symbol)(transform(rhs2))
                debuglog("forwarding result1: " + rhs1)
                debuglog("forwarding result2: " + rhs2)
                debuglog("forwarding result3: " + rhs3)
                val defdef = localTyper.typed(deriveDefDef(tree)(_ => rhs3))
                defdef

              // specialize an existing method body
              case Some(SpecializedImplementationOf(target)) =>
                debuglog("")
                debuglog("Generating specialized implmentation for: " + tree)
                val tree1: Tree = addDefDefBody(ddef, target)
                val tree2: Tree = deriveDefDef(tree1)(rhs => atOwner(tree1.symbol)(transform(rhs)))
                debuglog("before rewiring: " + tree1)
                debuglog("after rewriting: " + tree2)
                localTyper.typed(tree2)

              // abstract methods
              case Some(Interface | _ : DeferredTypeTag | _: TypeTagParam) =>
                tree

              // deferred type tag implementation
              case Some(DeferredTypeTagImplementation(tparam)) =>
                val tagTrees = typeTagTrees()
                val localTParam = tparam.tpe.asSeenFrom(currentClass.info.prefix, currentClass).typeSymbol
                localTyper.typed(deriveDefDef(tree)(_ => localTyper.typed(tagTrees(localTParam))))

              // uh-oh
              case Some(info) =>
                sys.error("Unknown info type: " + info)

              case None =>
                localTyper.typed(deriveDefDef(tree)(rhs => atOwner(tree.symbol)(transform(rhs))))
            }

          // force the normalized members
          afterMiniboxInject(sym.info)

          if (metadata.isNormalStem(sym)) {
            // collect method body, it will be necessary
            // for the normalized members
            MethodBodiesCollector(ddef)
            val normSymbols = metadata.normalOverloads(sym).values.toList.sortBy(_.defString).filterNot(_ == sym)

            val normTrees =
              for (m <- normSymbols) yield
                transform(localTyper.typed(DefDef(m, (_: List[List[Symbol]]) => EmptyTree)))
            res :: normTrees
          } else {
            res
          }

        case vdef @ ValDef(mods, name, tpt, EmptyTree) if hasInfo(vdef) =>
          memberSpecializationInfo(tree.symbol) match {
            case SpecializedImplementationOf(original) =>
              val newTree = addValDefBody(tree, original)
              localTyper.typed(deriveValDef(newTree)(rhs => atOwner(newTree.symbol)(transform(rhs))))
            case TypeTagParam(_) =>
              Descend
            case info =>
              sys.error("Unknown info type: " + info)
          }

        // rewiring new class construction
        // new C[Int] => new C_J[Int]
        case New(cl) =>
          val newType = miniboxQualifier(tree.pos, cl.tpe)
          localTyper.typedQualifier(New(TypeTree(newType)))

        // rewiring this calls
        // C.this.foo => C_J.this.foo
        case This(cl) if metadata.isClassStem(tree.symbol) && !currentOwner.ownerChain.contains(tree.symbol) =>
          val newType = miniboxQualifier(tree.pos, tree.tpe)
          val res = localTyper.typed(This(newType.typeSymbol))
          res

        // rewire member selection
        case Select(oldQual, mbr) if (metadata.miniboxedTParamFlag(extractQualifierType(oldQual).typeSymbol) || oldQual.isInstanceOf[Super] || metadata.memberOverloads.isDefinedAt(tree.symbol)) =>
          val oldMbrSym = tree.symbol
          val oldQualTpe: Type = extractQualifierType(oldQual)
          val oldQualSym = tree.symbol.owner //oldQualTpe.typeSymbol
          val (newQual: Tree, newQualTpe: Type, newQualSym: Symbol) =
            oldQual match {
              case sup @ Super(ths, name) =>
                if (!name.isEmpty) {
                  // The same code appears in Duplicators, but de-duplicating it is too much work :(
                  global.reporter.error(tree.pos, "Using named super is not supported in miniboxed classes. " +
                                                  "For a workaround, please see " +
                                                  "https://github.com/miniboxing/miniboxing-plugin/issues/166:")
                  (localTyper.typed(gen.mkMethodCall(definitions.Predef_???, Nil)).setType(ErrorType), ErrorType, oldQualSym)
                } else {
                  // rewriting member selection on the fly, otherwise this won't work :(
                  val selTree = localTyper.typedOperator(Select(Super(transform(ths), sup.mix), mbr))
                  val Select(supTree, _) = selTree
                  val ntpe = supTree.tpe baseType selTree.symbol.owner
                  (supTree, ntpe, selTree.symbol.owner)
                }
              case _ =>
                val nqual = transform(oldQual)
                val ntpe = extractQualifierType(nqual)
                (nqual, ntpe, ntpe.typeSymbol)
            }

          val spec = extractSpec(oldQual.pos, oldQual.tpe, currentOwner)

          // patching for the corresponding symbol in the new receiver
          // new C[Int].foo => new C_J[Int].foo
          // note: constructors are updated in the next step, as their class overloads are recorded
          val newMbrSym =
            if ((oldQualSym != newQualSym) &&
                (metadata.getClassStem(newQualSym) == oldQualSym) &&
                (oldMbrSym.name != nme.CONSTRUCTOR)) {
              equivalentMemberInSpecializedClass(oldMbrSym, newQualSym).getOrElse(oldMbrSym)
            } else
              oldMbrSym

          // patching according to the receiver's specialization
          // new C[Int].foo_J => new C_J[Int].foo_J
          val specMbrSym = {
            val tpe1 = newQualTpe baseType (newMbrSym.owner)
            extractSpec(tree.pos, tpe1, currentOwner) match { // Get the partial specialization
              case Some(pspec) if metadata.memberOverloads.get(newMbrSym).flatMap(_.get(pspec)).isDefined =>
                val newMethodSym = metadata.memberOverloads(newMbrSym)(pspec)
                newMethodSym
              case _ =>
                newMbrSym
            }
          }

          val tpe1 = newQualTpe baseType (newMbrSym.owner)
          val ntree = localTyper.typedOperator(gen.mkAttributedSelect(newQual, specMbrSym))
          assert(!metadata.dummyConstructors(ntree.symbol), "dummy constructor: " + ntree.symbol.defString + " left in tree " + tree)
          ntree

        case tapply @ TypeApply(oldFun, targs) =>
          afterMiniboxInject(oldFun.symbol.owner.info)
          val oldSym = oldFun.symbol
          val oldType = tapply.tpe
          val newFun = transform(oldFun)
          val newSym = newFun.symbol

          // find the normalized member
          val pos = if (tree.pos != NoPosition) tree.pos else oldFun.pos // infered arguments get NoPosition
          val normSym =
            extractNormSpec(pos, targs.map(_.tpe), newFun.symbol, currentOwner) match {
              case Some(newSym) => newSym
              case None => newFun.symbol
            }

          // replace the member by the normalized member
          val ntargs = targs.map(transform)
          val tree1 =
            if (normSym != newSym)
              newFun match {
                case Ident(_) =>
                  TypeApply(Ident(newSym), ntargs)
                case Select(newQual, _) =>
                  TypeApply(Select(newQual, normSym), ntargs)
              }
            else
              TypeApply(newFun, ntargs)

          val tree2 = localTyper.typedOperator(tree1)
          tree2

        case Apply(oldFun, oldArgs) =>
          val args = oldArgs.map(transform(_))
          val oldMethodSym = oldFun.symbol
          val oldMethodTpe = oldFun.tpe
          val newFun = transform(oldFun)
          val newMethodSym = newFun.symbol
          val newMethodTpe = newFun.tpe

          // 1. Get the type arguments
          val targs = newFun match {
            case TypeApply(_, targs) => targs
            case _ => Nil
          }

          // 1. Generate type tags
          val (tags, argTypes) = tagUtils.separateTypeTagArgsInType(newMethodSym.info, targs.map(_.tpe))

          def tparamsToTargs(tree: Tree): List[(Symbol, Symbol)] = {
            tree match {
              case TypeApply(qual, targs) =>
                (qual.symbol.typeParams zip targs.map(_.tpe.typeSymbol)) ::: tparamsToTargs(qual)
              case Select(qual, _) =>
                val qualTpe = qual.tpe
                val owner = tree.symbol.owner
                afterMiniboxInject(owner.info)
                var ownerTpe = qualTpe.baseType(owner)
                if (ownerTpe == NoType)
                    ownerTpe = qualTpe.underlying.baseType(owner)

                owner.typeParams zip ownerTpe.typeArgs.map(_.typeSymbol)
              case _ =>
                Nil
            }
          }

          //  local type tags ==> type parameters ==> type arguments ==> type tags from scope  //

          //  local type tags ==> type parameters
          val localTags  = metadata.localTypeTags.getOrElse(newMethodSym, HashMap()).toMap
          val normTags   = metadata.normalTypeTags.getOrElse(newMethodSym, HashMap()).toMap
          val tagToTpar  = localTags ++ normTags

          // type parameters ==> type arguments
          val tparToTarg = tparamsToTargs(newFun).toMap

          // type arguments ==> type tags from scope
          val targToTag  = typeTagTrees()

          val tagArgs =
              for (tag <- tags) yield
                try {
                  val tpar = tagToTpar(tag)
                  val targ = tparToTarg.getOrElse(tpar, tpar)
                  val tag2 = targToTag(targ)
                  tag2
                } catch {
                  case ex: Exception =>
                    reporter.error(tree.pos,
                        s"""|[miniboxing plugin internal error]
                            |Type tag not found:
                            |  before:     $oldFun (of ${oldMethodSym.defString})
                            |  after:      $newFun (of ${newMethodSym.defString}
                            |  tags:       $tags
                            |  tagToTpar:  $tagToTpar
                            |  tparToTarg: $tparToTarg
                            |  targToTag:  $targToTag""".stripMargin)
                    gen.mkMethodCall(Predef_???, Nil)
                }

          val tree1 = gen.mkMethodCall(newFun, tagArgs ::: args)
          val tree2 =
            localTyper.silent(_.typed(tree1)) match {
              case global.analyzer.SilentResultValue(t: Tree) => t
              case global.analyzer.SilentTypeError(err) =>
                global.reporter.warning(err.errPos, "Miniboxing redirection error at " + currentOwner.fullName + ":\n" +
                                                     err.toString() + "\nNote: The resulting bytecode will be incorrect " +
                                                     "due to this error, so please don't use it in production and " +
                                                     "report the bug to https://github.com/miniboxing/miniboxing-plugin/issues.")
                gen.mkMethodCall(Predef_???, Nil)
            }

          tree2

        case _ =>
          Descend
      }
    }

    def extractNormSpec(pos: Position, targs: List[Type], target: Symbol, owner: Symbol = currentOwner): Option[Symbol] = {
      val tparams = afterMiniboxInject(target.info).typeParams
      assert(tparams.length == targs.length, "Type parameters and args don't match for call to " + target.defString + " in " + owner.defString + ": " + targs.length)
      val spec = PartialSpec.fromTargs(pos, tparams, targs, currentOwner)
      metadata.normalOverloads.get(target).flatMap(_.get(spec))
    }

    def extractSpec(pos: Position, qualTpe: Type, owner: Symbol = currentOwner): Option[PartialSpec] = {
      val res = qualTpe match {
        case ThisType(cls) if metadata.isClassStem(cls) =>
          metadata.classSpecialization.get(cls)
//        case SuperType(ThisType(cls), thatTpe) =>
//          if (metadata.isClassStem(cls))
//            None
//          else
//            extractSpec(thatTpe, owner)
        case SingleType(pre, x) =>
          extractSpec(pos, qualTpe.widen, owner)
        case PolyType(tparams, rest) =>
          extractSpec(pos, rest, owner)
        case TypeRef(pre, clazz, targs) =>
          val tparams = afterMiniboxInject(metadata.getClassStem(clazz).orElse(clazz).info).typeParams
          Some(PartialSpec.fromTargs(pos, tparams, targs, owner))
        case _ =>
          // unknown
          None
      }
      res
    }

    def ltypedpos(tree: Tree): Tree =
      localTyper.typedPos(curTree.pos)(tree)

    def ltyped(tree: Tree): Tree =
      localTyper.typed(tree)


    // We collect the bodies of the target methods in order to have them available
    // for copying inside the methods that are specialized implementations of them.
    private object MethodBodiesCollector extends Traverser {
      private val body = HashMap[Symbol, (Tree, List[List[Symbol]])]()

      override def traverse(tree: Tree) = tree match {
        case DefDef(_, _, _, vparamss, _, rhs) if (metadata.templateMembers(tree.symbol)) =>
          collect(tree.symbol, rhs, vparamss.map(_.map(_.symbol)))
        case ValDef(mods, name, tpt, rhs) if metadata.templateMembers(tree.symbol) =>
          collect(tree.symbol, rhs, Nil)
        case _ =>
          super.traverse(tree)
      }

      private def collect(member: Symbol, rhs: Tree, params: List[List[Symbol]]) = {
        body(member) = (rhs.duplicate, params)
        metadata.templateMembers -= member
        debug("collected " + member.fullName + " -- " + member + ":")
        debug("  " + rhs)
      }

      def getMethodBody(meth: Symbol, owner: Symbol) =
        try {
          body.apply(meth)
        } catch {
          case e: Throwable =>
            global.reporter.error(meth.pos, "Internal miniboxing plugin error: Method body for " + meth + " not collected (when duplicating into " + owner + ")")
            (localTyper.typed(gen.mkMethodCall(Predef_???, Nil)), meth.paramss)
        }
      def getFieldBody(fld: Symbol, owner: Symbol) = getMethodBody(fld, owner)._1
    }

    private def prepareDuplicator(newMethod: Option[Symbol], oldMethod: Option[Symbol], newClass: Symbol, oldClass: Symbol): Duplicator = {

      if (metadata.getClassStem(newClass) != NoSymbol)
        assert(metadata.getClassStem(newClass) == oldClass)

      val tparamUpdate: Map[Symbol, Symbol] = metadata.getClassStem(newClass).typeParams.zip(newClass.typeParams).toMap

      // specialization environment
      val senv1: List[(Symbol, Symbol)] = metadata.getClassStem(newClass).typeParams.zip(newClass.typeParams)
      val sspec = metadata.classSpecialization.getOrElse(newClass, Map())
      val senv2 =
        for ((oldTarg, newTarg) <- senv1) yield
          sspec.getOrElse(oldTarg, Boxed) match {
            case Boxed  => (oldTarg, newTarg.tpeHK)
            case mboxed => (oldTarg, storageType(newTarg, mboxed))
          }
      val senv3 = senv2.toMap

      // normalization environment
      val nenv3 =
        (newMethod, oldMethod) match {
          case (Some(symbol), Some(source)) =>
            val normStem = metadata.getNormalStem(symbol)
            val nenv1: List[(Symbol, (Symbol, Symbol))] = source.typeParams zip (symbol.typeParams zip normStem.typeParams)
            val nspec = metadata.normalSpecialization.getOrElse(symbol, Map())
            val nenv2 =
              for ((oldTarg, (newTarg, stemTarg)) <- nenv1) yield
                nspec.getOrElse(stemTarg, Boxed) match {
                  case Boxed => (oldTarg, newTarg.tpeHK)
                  case mboxed => (oldTarg, storageType(newTarg, mboxed))
                }
            nenv2.toMap
          case _ =>
            Map.empty
        }

      val miniboxedEnv: Map[Symbol, Type] = senv3 ++ nenv3
      val miniboxedTypeTags = typeTagTrees(newMethod.getOrElse(newClass))

      val mbSubst = typeMappers.MiniboxSubst(miniboxedEnv)
      val d =
        new Duplicator(Map.empty,
                       oldMethod.map(_.enclClass).getOrElse(oldClass),
                       newMethod.map(_.enclClass).getOrElse(newClass),
                       mbSubst.shallowSubst,
                       mbSubst.deepSubst
        )
      d
    }

    private def duplicateBody(tree0: Tree, source: Symbol): Tree = {

      val symbol = tree0.symbol
      val d = prepareDuplicator(Some(tree0.symbol), Some(source), tree0.symbol.owner, source.owner)

      // Duplicator chokes on retyping new C if C is marked as abstract
      // but we need this in the backend, else we're generating invalid
      // flags for the entire class - for better or worse we adapt just
      // before calling the duplicator, and get back for specialization
      preMiniboxingFlags()

      val tree1 =
        reportError(symbol)(
          d.retyped(
            localTyper.context1,
            tree0
          )
        )(_ => {
//          println(tree)
          localTyper.typed(deriveDefDef(tree0)(_ => localTyper.typed(gen.mkMethodCall(Predef_???, Nil))))
        })
      // get back flags
      postMiniboxingFlags()

      tree1
    }

    /** Put the body of 'source' as the right hand side of the method 'tree'.
     *  The destination method gets fresh symbols for type and value parameters,
     *  and the body is updated to the new symbols, and owners adjusted accordingly.
     *  However, if the same source tree is used in more than one place, full re-typing
     *  is necessary. @see method duplicateBody
     */
    private def addDefDefBody(tree: DefDef, source: Symbol): Tree = {
      val symbol = tree.symbol
      debuglog("specializing body of " + symbol.defString)
      val (tparams, tags, vparams, tpt) = tree match {
        case DefDef(_, _, tparams, tvparams :: Nil, tpt, _) =>
          val (ttags, vparams) = tagUtils.separateTypeTagArgsInTree(tvparams)
          (tparams, ttags, vparams, tpt)
      }

//      val env = metadata.getClassStem(symbol).typeParams.zip(symbol.typeParams.map(_.tpeHK)).toMap
//      val boundTvars = env.keySet
      val origtparams = source.typeParams //.filter(tparam => !boundTvars(tparam) || !isPrimitiveValueType(env(tparam)))
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
      val (body, parameters) = MethodBodiesCollector.getMethodBody(source, symbol.owner)

      val symSubstituter = new TreeSymSubstituter(
        parameters.flatten ::: origtparams,
        vparams.map(_.symbol) ::: newtparams)

      // bug #149: the TreeSymSubstituter forces .info on classes that will be completely removed by the
      // miniboxing transformation (after being duplicated + miniboxed), so we get spurious warnings that
      // code could not be specialized. The fix is to execute the symbol substituter before inject:
      val newBody = beforeMiniboxInject(symSubstituter(body.duplicate))
      tpt.setType(tpt.tpe.substSym(oldtparams, newtparams))

      val meth = copyDefDef(tree)(rhs = newBody)
      duplicateBody(meth, source)
    }

    private def addValDefBody(tree: Tree, origMember: Symbol): Tree = {
      val valSymbol = tree.symbol
      val origBody = MethodBodiesCollector.getFieldBody(origMember, valSymbol);
      val origClass = origMember.owner

      val tree1 = deriveValDef(tree)(_ => origBody.duplicate)
      debuglog("now typing: " + tree1 + " in " + tree.symbol.owner.fullName)

      duplicateBody(tree1, origMember)
    }

    // Create bodies for members in a tree
    def createMemberTree(mbr0: Option[Symbol]): Option[Tree] = mbr0 map {
      mbr =>
        debuglog("creating empty tree for " + mbr.fullName)
        val tree =
          if (mbr.isMethod)
            DefDef(mbr, { (_: List[List[Symbol]]) => EmptyTree })
          else if (mbr.isValue)
            ValDef(mbr, EmptyTree).setType(NoType)
          else {
            reporter.error(mbr.pos, s"[miniboxing internal error] Unable to generate member: " + mbr.defString)
            gen.mkMethodCall(Predef_???, Nil)
          }
        val tree2 = atPos(mbr.pos)(tree)
        val tree3 = localTyper.typed(tree2)
        tree3
    }
  }
}
