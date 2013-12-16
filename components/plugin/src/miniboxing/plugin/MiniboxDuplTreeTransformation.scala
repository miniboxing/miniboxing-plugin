package miniboxing.plugin

import scala.reflect.internal.Flags
import scala.tools.nsc.transform.TypingTransformers
import scala.collection.mutable.Set
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.typechecker._

trait MiniboxDuplTreeTransformation extends TypingTransformers {
  self: MiniboxDuplComponent =>

  import global._
  import global.definitions._
  import Flags._
  import typer.{ typed, atOwner }
  import memberSpecializationInfo._

  /** Generate the code to convert a miniboxed value to its boxed representation */
  def convert_minibox_to_box(tree: Tree, tpe: Type, currentOwner: Symbol): Tree = {
    val tpe1 = tree.tpe
    val tags = typeTagTrees(currentOwner)
    // sanity checks
    assert(tpe1 == LongTpe, tree + " expecting .tpe to be Long, not " + tpe + ".")
    assert(tags.isDefinedAt(tpe.typeSymbol), tpe + " (" + showRaw(tpe) + ") does not correspond to a tag: " + tags + " in " + currentOwner)

    gen.mkMethodCall(minibox2box, List(tpe), List(tree, tags(tpe.typeSymbol)))
  }

  /** Generate the code to convert a miniboxed value to its boxed representation */
  def convert_box_to_minibox(tree: Tree, currentOwner: Symbol): Tree = {
    val tpe = tree.tpe
    val tags = typeTagTrees(currentOwner)
    // sanity checks
    // this is a wrong assumption, violated when overriding miniboxed type params by long:
    // assert(tpe != LongTpe, tree + " expecting .tpe to be Tsp, not Long.")
    assert(tags.isDefinedAt(tpe.typeSymbol), tpe + " (" + showRaw(tpe) + ") does not correspond to a tag: " + tags + " in " + currentOwner)

    gen.mkMethodCall(box2minibox, List(tpe), List(tree, tags(tpe.typeSymbol)))
  }

  var ttindent = 0

  // TODO: This should be:
  // 1) lazy in computing the trees
  // 2) moved over to miniboxing logic, as it's not specific to trees
  def typeTagTrees(owner: Symbol): Map[Symbol, Tree] = {
    ttindent += 1
//    println("  " * ttindent + " <= type tag trees for " + owner + " owner chain: " + owner.ownerChain.reverse)
    val prev = if (owner.owner != NoSymbol) typeTagTrees(owner.owner) else Map.empty
    val res = prev ++
      inheritedDeferredTypeTags.getOrElse(owner, Map.empty).map({case (method, t) => (t, { gen.mkMethodCall(method, List())})}) ++
      primaryDeferredTypeTags.getOrElse(owner, Map.empty).map({case (method, t) => (t, { gen.mkMethodCall(method, List())})}) ++
      globalTypeTags.getOrElse(owner, Map.empty).map({case (t, tag) => (t, gen.mkAttributedSelect(gen.mkAttributedThis(tag.owner),tag))}) ++
      localTypeTags.getOrElse(owner, Map.empty).map({case (t, tag) => (t, Ident(tag))}) ++
      standardTypeTagTrees // override existing type tags
//    println("  " * ttindent + " => type tag trees for " + owner + ": " + res)
    ttindent -= 1
    res
  }

  /**
   * The tree transformer that adds the trees for the specialized classes inside
   * the current package.
   */
  class MiniboxTreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    import global._

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
      val global: MiniboxDuplTreeTransformation.this.global.type = MiniboxDuplTreeTransformation.this.global
      val miniboxing: MiniboxDuplComponent { val global: MiniboxDuplTreeTransformation.this.global.type } =
        MiniboxDuplTreeTransformation.this.asInstanceOf[MiniboxDuplComponent { val global: MiniboxDuplTreeTransformation.this.global.type }]
    } with miniboxing.plugin.Duplicators {
      private val (castfrom, castto) = casts.unzip
      private object CastMap extends SubstTypeMap(castfrom.toList, castto.toList)

      class BodyDuplicator(_context: Context) extends super.BodyDuplicator(_context) {
        override def castType(tree: Tree, pt: Type): Tree = {
          // log(" expected type: " + pt)
          // log(" tree type: " + tree.tpe)
          // tree.tpe = if (tree.tpe != null) fixType(tree.tpe) else null
          // log(" tree type: " + tree.tpe)
//          val ntree = if (tree.tpe != null && !(tree.tpe <:< pt)) {
//            val casttpe = CastMap(tree.tpe)
//            if (casttpe <:< pt) gen.mkCast(tree, casttpe)
//            else if (casttpe <:< CastMap(pt)) gen.mkCast(tree, pt)
//            else tree
//          } else tree
//          ntree.tpe = null
          tree.tpe = null
          tree
        }
      }

      protected override def newBodyDuplicator(context: Context) = new BodyDuplicator(context)
    }

    def typeTagTrees(member: Symbol = currentOwner) =
      MiniboxDuplTreeTransformation.this.typeTagTrees(member)

    override def transform(tree: Tree): Tree = miniboxTransform(tree)

    def miniboxQualifier(pos: Position, tpe: Type): Type = {
      val oldClass = tpe.typeSymbol
      val newClass =
        extractSpec(tpe, currentMethod, currentClass) match {
          case Some(pspec) =>
            specializedClasses(oldClass)(pspec)
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
        case New(cl)     => cl.tpe
        case This(clazz) => appliedType(tree.symbol, currentClass.typeParams.map(_.tpe): _*)
        case _ => tree.tpe
      }

    def extractFunctionQualifierType(tree: Tree): Type = tree match {
      case Select(newQual, _) => newQual.tpe
      case TypeApply(fun, _) => extractFunctionQualifierType(fun)
      case Apply(fun, _) => extractFunctionQualifierType(fun)
      case _ => NoType
    }

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

        case Template(parents, self, body) =>
          MethodBodiesCollector(tree)
          afterMinibox(tree.symbol.enclClass.info)

          //  This is either a class that has nothing to do with miniboxing or that is the base
          //  class (now trait) for the specialization.
          //  Also collect the bodies of the methods that need to be copied and specialized.
          val sym = tree.symbol.enclClass
          val decls = afterMinibox(sym.info).decls.toList

          // specialized classes inside this template
          val specClassesTpls = createSpecializedClassesTrees(body)
          val specClassesTped = specClassesTpls map localTyper.typed

          // members
          val specMembers = createMethodTrees(tree.symbol.enclClass) map localTyper.typed
          val bodyDefs =
            if (specializedBase(tree.symbol.enclClass))
              body.filter(defdef => decls.contains(defdef.symbol))
            else
              body
          val memberDefs = atOwner(currentOwner)(transformTrees(bodyDefs ::: specMembers ::: specClassesTped))

          // parents
          val parents1 = map2(sym.info.parents, parents)((tpe, parent) => TypeTree(tpe) setPos parent.pos)

          // new template def
          val templateDef = treeCopy.Template(tree, parents1, self, atOwner(currentOwner)(memberDefs))
          val tree1 = localTyper.typedPos(tree.pos)(templateDef)

          tree1

        // TODO: What I need
        // - rewire selects
        // - rewire typeapplys
        // - rewire applys
        // - constructors?
        // - super calls?

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
            case ForwardTo(target) =>
              val (ttagWrapperArgs, wrapperParams) = separateTypeTagArgsInTree(vparams)

              val rhs1 = gen.mkMethodCall(target, tparams.map(_.symbol.tpeHK), wrapperParams.map(param => Ident(param.symbol)))
              val rhs2 = localTyper.typed(rhs1)
              super.transform(localTyper.typed(deriveDefDef(tree)(_ => rhs2)))

            // copy the body of the `original` method
            case SpecializedImplementationOf(target) =>
              // we have an rhs, specialize it
              def reportTypeError(body: =>Tree) = reportError(body)(_ => ddef)
              val tree1 = specializeDefDefBody(ddef, target)
              debuglog("implementation: " + tree1)
              super.transform(tree1)

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
          res

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

        // rewiring new class construction
        // new C[Int] => new C_J[Int]
        case New(cl) if specializedClasses.isDefinedAt(cl.tpe.typeSymbol) =>
          val newType = miniboxQualifier(tree.pos, cl.tpe)
          localTyper.typedOperator(New(TypeTree(newType)))

        // rewiring this calls
        // C.this.foo => C_J.this.foo
        case This(cl) if specializedClasses.isDefinedAt(tree.symbol) =>
          val newType = miniboxQualifier(tree.pos, tree.tpe)
          localTyper.typed(This(newType.typeSymbol))

//        case Select(Super(ths, name), member) =>
//          println("retyping super: " + tree)
//          val tree1 = localTyper.typedOperator(Select(Super(transform(ths), name), member))
//          // catch the overloaded thief red-handed next time
//          if (tree1.symbol.isOverloaded) {
//            for (sym <- tree1.symbol.alternatives)
//              println(sym.defString + "   " + sym.ownerChain)
//            println(currentClass.baseClasses)
//          }
//          tree1

        // rewiring member selection
        case Select(oldQual, mbr) if extractQualifierType(oldQual).typeSymbol.hasFlag(MINIBOXED) =>
          val oldMbrSym = tree.symbol
          val oldQualTpe: Type = extractQualifierType(oldQual)
          val newQual = transform(oldQual)
          val newQualTpe: Type = extractQualifierType(newQual)

          val spec = extractSpec(oldQual.tpe, currentMethod, currentClass)

          // new C[Int] => new C_J[Int]
          // new C[Int].C#<init> => new C_J[Int].C_J#<init>
          val oldQualSym = oldQualTpe.typeSymbol
          val newQualSym = newQualTpe.typeSymbol

          // patching according to the new qualifier
          val newMbrSym =
            if (oldQualSym != newQualSym) {
              val pspec = partialSpec(newQualSym)
              assert(specializedClasses(oldQualSym).isDefinedAt(pspec))
              assert(overloads.isDefinedAt(oldMbrSym))
              assert(overloads(oldMbrSym).isDefinedAt(pspec))
              overloads(oldMbrSym)(pspec)
            } else
              oldMbrSym

          // patching according to the specialization
          val specMbrSym = {
            val tpe1 = newQualTpe baseType (newMbrSym.owner)
            extractSpec(tpe1, currentMethod, currentClass) match { // Get the partial specialization
              case Some(pspec) if !PartialSpec.isAllAnyRef(pspec) && overloads.get(newMbrSym).flatMap(_.get(pspec)).isDefined =>
                val newMethodSym = overloads(newMbrSym)(pspec)
                newMethodSym
              case other =>
                newMbrSym
            }
          }

          //
          val ntree = localTyper.typedOperator(gen.mkAttributedSelect(newQual, specMbrSym))
//          println()
//          println("initial tree: " + tree + " : " + tree.tpe)
//          println("rewiring original: " + oldMbrSym.defString + " (onwer: " + oldMbrSym.owner + ")")
//          println("rewiring step 1:   " + newMbrSym.defString + " (onwer: " + newMbrSym.owner + ")")
//          println("rewiring step 2:   " + specMbrSym.defString + " (onwer: " + specMbrSym.owner + ")")
//          println(ntree.tpe)
          ntree

        case TypeApply(oldFun, targs) =>
          // TODO
          localTyper.typed(treeCopy.TypeApply(tree, transform(oldFun), targs.map(transform)))

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
          val (tagSyms, argTypes) = separateTypeTagArgsInType(newMethodSym.info, targs.map(_.tpe))
          val tagsToTparams1 = localTypeTags.getOrElse(newMethodSym, Map()).map(_.swap).toMap
          val tparamInsts = for (tagSym <- tagSyms) yield {
            try {
              val tparam = tagsToTparams1(tagSym)
              val instOwner = extractFunctionQualifierType(newFun).baseType(tparam.owner)
              val tparamFromQualToInst = (instOwner.typeSymbol.typeParams zip instOwner.typeArgs).toMap
              if (targs != null) assert(newMethodSym.info.typeParams.length == targs.length, "Type parameter mismatch in rewiring from " + oldMethodSym.defString + " to " + newMethodSym.defString + ": " + targs)
              val tparamFromNormToInst = if (targs != null) (newMethodSym.info.typeParams zip targs.map(_.tpe)).toMap else Map.empty
              val tparamToInst = tparamFromQualToInst ++ tparamFromNormToInst
              tparamToInst(tparam).typeSymbol
            } catch {
              case ex: Exception =>
                println("Tag not found:")
                println(newMethodSym.defString)
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

          val tree1 = gen.mkMethodCall(newFun, localTagArgs ::: args)
//          println()
//          println(tree + " ==> " + tree1)
//          println(tree1)
//          println(tree1.tpe)
//          println(newFun)
//          println(newFun.symbol.defString)
//          println(newFun.tpe)
          val tree2 = localTyper.typed(tree1)
//          println("after")
          tree2

//        // Super constructor call rewiring for:
//        //  - non-specialized classes
//        //  - specialized classes
//        case Select(sup @ Super(ths, name), member)
//          if (member != nme.CONSTRUCTOR) && {
//            afterMinibox(sup.tpe.typeSymbol.info)
//            (sup.symbol.info.parents != beforeMinibox(sup.symbol.info.parents)) || baseClass.isDefinedAt(sup.symbol) && (baseClass(sup.symbol) != sup.symbol)
//          } =>
//          val tree1 = localTyper.typedOperator(Select(Super(ths, name), member))
//          // catch the overloaded thief red-handed next time
//          if (tree1.symbol.isOverloaded) {
//            for (sym <- tree1.symbol.alternatives)
//              println(sym.defString + "   " + sym.ownerChain)
//            println(currentClass.baseClasses)
//          }
//          super.transform(tree1)
//
//        // Redirect method calls without type parameters
//        case Apply(sel @ Select(qual, fn), args) if {
//          afterMinibox(sel.symbol.owner.info)
//          base.isDefinedAt(tree.symbol) &&
//          (base(tree.symbol) == tree.symbol)
//        } =>
//          val oldMethodSym = tree.symbol
//          val oldMethodType = sel.tpe
//          val tree1 =
//            memberSpecializationInfo.get(currentMethod) match {
//              case Some(spec: ForwardTo) =>
//                // don't touch forwarders, they're correctly generated the first time
//                // although it may be interesting to use this logic to generate forwarders too
//                // TODO: Can this be done?
//                tree
//              case _ =>
//                val tpe1 = qual.tpe baseType (oldMethodSym.owner)
//                extractSpec(tpe1, currentMethod, currentClass) match { // Get the partial specialization
//                  case Some(pspec) if !PartialSpec.isAllAnyRef(pspec) && overloads.get(oldMethodSym).flatMap(_.get(pspec)).isDefined =>
//                    val newMethodSym = overloads(oldMethodSym)(pspec)
//                    val tree1 = rewiredMethodCall(qual, oldMethodSym, oldMethodType, newMethodSym, currentClass.info.memberInfo(newMethodSym), args, null)
//                    stats("redirecting method call: " + tree + " ==> " + tree1)
//                    tree1
//                  case other =>
//                    tree
//                }
//            }
//          super.transform(tree1)
//
//        // Super constructor call rewiring for:
//        //  - non-specialized classes
//        //  - specialized classes
//        case Apply(sel @ Select(sup @ Super(ths, name), nme.CONSTRUCTOR), args)
//          if {
//            afterMinibox(sup.tpe.typeSymbol.info)
//            // either the parents changed, or class is specialized (specialized classes
//            // don't exist before minibox, thus their parents don't *change* but *appear*)
//            (sup.symbol.info.parents != beforeMinibox(sup.symbol.info.parents)) || baseClass.isDefinedAt(sup.symbol) && (baseClass(sup.symbol) != sup.symbol) &&
//            // don't rewire super constructor if the parent is AnyRef
//            // TODO: Shouldn't this be done for any non-specialized class?
//            !(sup.symbol.info.parents.head =:= AnyRefTpe)
//          } =>
//            val oldInitSym = sel.symbol
//            val oldInitTpe = sel.tpe // do we want the instantiated type?
//            // Someday I will rewrite that damn typer! I spend one entire day to find out that a
//            // Select(Super(_, _), _) is typed differently from a Super(_, _)...
//            val init = localTyper.typedOperator(Select(Super(ths, name) setPos sup.pos, nme.CONSTRUCTOR))
//            val newInitSym = init.symbol
//            val newInitTpe = currentClass.info.memberInfo(newInitSym)
//            if (newInitSym == oldInitSym)
//              tree
//            else {
//              val Select(qual, _) = init
//              val tree1 = rewiredMethodCall(qual, oldInitSym, oldInitTpe, newInitSym, newInitTpe, args.map(transform), null)
//              localTyper.typed(tree1)
//            }
//
//        // Constructor call rewiring for specialized classes
//        //   new C[Int](3)              => new C_J[Int](INT, 3.toInt)
//        //   <interface>.this.<init>(3) => <specialized>.this.<init>(...)
//        case Apply(ctor @ Select(qual, nme.CONSTRUCTOR), args) if {
//          qual match {
//            case New(cl) => afterMinibox(cl.symbol.info)
//            case _ =>
//          }
//          specializedClasses.isDefinedAt(qual.tpe.typeSymbol) } =>
//
//          val oldClassCtor = ctor.symbol
//          val cltpe: Type  = qual match {
//            case New(cl) => cl.tpe
//            case This(clazz) => appliedType(qual.symbol, currentClass.typeParams.map(_.tpe): _*)
//          }
//          val tree1 = cltpe match {
//            case TypeRef(pre, oldClass, targs) =>
//              val spec = extractSpec(cltpe, currentMethod, currentClass)
//              def newQual(newClass: Symbol) =
//                qual match {
//                  case New(_)  => localTyper.typedOperator(New(TypeTree(typeRef(pre, newClass, targs))))
//                  case This(_) => localTyper.typed(This(newClass))
//                }
//              spec match {
//                case Some(pspec) if !PartialSpec.isAllAnyRef(pspec) =>
//                  assert(specializedClasses(oldClass).isDefinedAt(pspec))
//                  assert(overloads.isDefinedAt(ctor.symbol))
//                  assert(overloads(ctor.symbol).isDefinedAt(pspec))
//                  val newClass = specializedClasses(oldClass)(pspec)
//                  val newClassCtor = overloads(oldClassCtor)(pspec)
//                  val tree1 = rewiredMethodCall(newQual(newClass), oldClassCtor, ctor.tpe, newClassCtor, currentClass.info.memberInfo(newClassCtor), args.map(transform), null)
//                  stats("redirecting new: " + tree + " ==> " + tree1)
//                  tree1
//                case Some(_) =>
//                  val allAnyRefSpec = oldClass.typeParams.filter(_ hasFlag MINIBOXED).map(t => (t, Boxed)).toMap
//                  val newClass = specializedClasses(oldClass)(allAnyRefSpec)
//                  val newClassCtor = overloads(oldClassCtor)(allAnyRefSpec)
//                  gen.mkMethodCall(newQual(newClass), newClassCtor, List(), args.map(transform))
//                case None =>
//                  global.reporter.error(tree.pos, "Unable to rewire constructor, this will probably lead to invalid bytecode.")
//                  tree
//              }
//            case _ =>
//              global.reporter.error(tree.pos, "Unsupported new operation.")
//              tree
//          }
//          localTyper.typed(tree1)
//
//        // Redirect method calls with type parameters
//        case Apply(tapply @ TypeApply(sel @ Select(qual, fn), targs), args) =>
//          afterMinibox(sel.symbol.owner.info)
//          val oldMethodSym = tree.symbol
//          val oldMethodType = tapply.tpe
//          val tree1 =
//            memberSpecializationInfo.get(currentMethod) match {
//              case Some(spec: ForwardTo) =>
//                // don't touch forwarders, they're correctly generated the first time
//                // although it may be interesting to use this logic to generate forwarders too
//                // TODO: Can this be done?
//                tree
//              case _ =>
//                val tpe1 = qual.tpe baseType (oldMethodSym.owner)
//                // rewiring based on the receiver
//                val newMethodSym =
//                  if (base.isDefinedAt(tree.symbol) && base(tree.symbol) == tree.symbol)
//                    extractSpec(tpe1, currentMethod, currentClass) match { // Get the partial specialization
//                      case Some(pspec) if !PartialSpec.isAllAnyRef(pspec) && overloads.get(oldMethodSym).flatMap(_.get(pspec)).isDefined =>
//                        val newMethod = overloads(oldMethodSym)(pspec)
//                        stats("redirecting method call: " + tree + " ==> " + newMethod.defString)
//                        newMethod
//                      case other =>
//                        oldMethodSym
//                    }
//                  else
//                    oldMethodSym
//
//                // rewiring based on the normalized member
//                val normMethodSym =
//                  extractNormSpec(targs.map(_.tpe), newMethodSym, currentMethod, currentClass) match {
//                    case Some(newSym) => newSym
//                    case None => newMethodSym
//                  }
//
//                // final tree
//                if (normMethodSym != oldMethodSym)
//                  rewiredMethodCall(transform(qual), oldMethodSym, oldMethodType, normMethodSym, currentClass.info.memberInfo(normMethodSym), args.map(transform), targs)
//                else
//                  super.transform(tree)
//            }
//          tree1
//
//        // Redirect field selections
//        case Select(qual, fn) if { afterMinibox(tree.symbol.owner.info); base.isDefinedAt(tree.symbol) && base(tree.symbol) == tree.symbol && !tree.symbol.isMethod } =>
//          val oldMethodSym = tree.symbol
//          val oldMethodType = tree.tpe
//          val tree1 =
//            memberSpecializationInfo.get(currentMethod) match {
//              case Some(spec: ForwardTo) =>
//                // don't touch forwarders, they're correctly generated the first time
//                // although it may be interesting to use this logic to generate forwarders too
//                // TODO: Can this be done?
//                tree
//              case _ =>
//                val tpe1 = qual.tpe baseType (oldMethodSym.owner)
//                extractSpec(tpe1, currentMethod, currentClass) match { // Get the partial specialization
//                  case Some(pspec) if !PartialSpec.isAllAnyRef(pspec) && overloads.get(oldMethodSym).flatMap(_.get(pspec)).isDefined =>
//                    val newMethodSym = overloads(oldMethodSym)(pspec)
//                    val tree1 = rewiredMethodCall(qual, oldMethodSym, oldMethodType, newMethodSym, currentClass.info.memberInfo(newMethodSym), null, null)
//                    stats("redirected selection: " + tree + " ==> " + tree1)
//                    tree1
//                  case other =>
//                    tree
//                }
//            }
//          super.transform(tree1)

        // Error on accessing non-existing fields
        case sel@Select(ths, field) if (ths.symbol ne null) && (ths.symbol != NoSymbol) && { afterMinibox(ths.symbol.info); specializedBase(ths.symbol) && (sel.symbol.isValue && !sel.symbol.isMethod) } =>
          unit.error(sel.pos, "The program is accessing field " + sel.symbol.name + " of miniboxed class (or trait) " + ths.symbol.name + ", a pattern which becomes invalid after the miniboxing transformation. Please allow Scala to generate getters (and possibly setters) by using val (or var) without the \"private[this]\" qualifier: " + (if (sel.symbol.isMutable) "var " else "val ") + sel.symbol.name + ": " + sel.symbol.info + "\".")
          localTyper.typed(gen.mkAttributedRef(Predef_???))

        case _ =>
          super.transform(tree)
      }
    }

    def extractNormSpec(targs: List[Type], target: Symbol, inMethod: Symbol, inClass: Symbol): Option[Symbol] = {
      val pSpecFromBaseClass = partialSpec.getOrElse(inClass, Map.empty)
      val mapTpar = typeEnv.getOrElse(inClass, Map.empty)
      val pSpecInCurrentClass = pSpecFromBaseClass.map({ case (tp, status) => (mapTpar.getOrElse(tp, tp.tpe).typeSymbol, status)})
      val pSpecInCurrentMethod = inMethod.ownerChain.filter(_.isMethod).flatMap(normalSpec.getOrElse(_, Map.empty))
      val pSpec = pSpecInCurrentClass ++ pSpecInCurrentMethod

      if (normbase.isDefinedAt(target)) {
        val tparams = afterMinibox(target.info).typeParams
        assert(tparams.length == targs.length, "Type parameters and args don't match for call to " + target.defString + " in " + inMethod + " of " + inClass + ": " + targs.length)
        val spec = (tparams zip targs) flatMap { (pair: (Symbol, Type)) =>
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
            case (p, tpe) =>
              if (pSpec.isDefinedAt(tpe.typeSymbol))
                Some((p, pSpec(tpe.typeSymbol)))
              else
                Some((p, Boxed))
          }
        }
        val pspec = spec.toMap
//        println()
//        println("rewiring target: " + target.defString)
//        println(pspec)
//        println(normalizations.get(target))
        if (!notSpecializable(target.owner, target) && target.typeParams.exists(isSpecialized(target.owner, _))) {
          assert(normalizations.isDefinedAt(target), "No normalizations defined for " + target.defString + " in " + target.owner)
          assert(normalizations(target).isDefinedAt(pspec), "No good normalizations found for " + target.defString + " in " + target.owner + ": " + pspec + " in " + normalizations(target))
//          println(target.defString + " ==> " + normalizations(target)(pspec))
//          println(currentClass + "." + currentMethod)
          Some(normalizations(target)(pspec))
        } else
          None
      } else
        None
    }

    def extractSpec(qualTpe: Type, inMethod: Symbol, inClass: Symbol): Option[PartialSpec] = {
      val pSpecFromBaseClass = partialSpec.getOrElse(inClass, Map.empty)
      val mapTpar = typeEnv.getOrElse(inClass, EmptyTypeEnv)
      val pSpecInCurrentClass = pSpecFromBaseClass.map({ case (tp, status) => (mapTpar.getOrElse(tp, tp.tpe).typeSymbol, status)})
      val pSpecInCurrentMethod = inMethod.ownerChain.filter(_.isMethod).flatMap(normalSpec.getOrElse(_, Map.empty))
      val pSpec = pSpecInCurrentClass ++ pSpecInCurrentMethod
      qualTpe match {
        case ThisType(cls) if (cls == inClass) =>
          Some(pSpecFromBaseClass)
        case SingleType(pre, x) =>
          extractSpec(qualTpe.widen, inMethod, inClass)
        case PolyType(tparams, rest) =>
          extractSpec(rest, inMethod, inClass)
        case TypeRef(pre, clazz, args) =>
          import miniboxing.runtime.MiniboxConstants._
          val tparams = afterMinibox(baseClass.getOrElse(clazz, clazz).info).typeParams
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
              case (p, tpe) =>
                if (pSpec.isDefinedAt(tpe.typeSymbol))
                  Some((p, pSpec(tpe.typeSymbol)))
                else
                  Some((p, Boxed))
            }
          }
          Some(spec.toMap)
        case _ =>
          // unknown
          None
      }
    }

    def ltypedpos(tree: Tree): Tree =
      localTyper.typedPos(curTree.pos)(tree)

    def ltyped(tree: Tree): Tree =
      localTyper.typed(tree)

    /**
     * In `MiniboxInfoTransform` we create only symbols for methods.
     * Here we add empty bodies for them.
     */
    private def createMethodTrees(sClass: Symbol): List[Tree] = {
      val mbrs = new ListBuffer[Tree]
      // needs to keep the order for constructors:
      for (m <- sClass.info.decls.filter(_.isConstructor).toList ::: sClass.info.decls.filterNot(_.isConstructor).toList.sortBy(_.defString) if m hasFlag MINIBOXED) {
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
      val miniboxedEnv = typeEnv.getOrElse(symbol, EmptyTypeEnv)
      val miniboxedTypeTags = typeTagTrees(symbol)

      debug(s"duplicating tree: for ${symbol} based on ${source}:\n${tree}")

//      println("DUPLICATING")
//      println(miniboxedEnv)
//      println(tree)

      val d = new Duplicator(castmap)
      debuglog("-->d DUPLICATING: " + tree)

      // Duplicator chokes on retyping new C if C is marked as abstract
      // but we need this in the backend, else we're generating invalid
      // flags for the entire class - for better or worse we adapt just
      // before calling the duplicator, and get back for specialization
      for (clazz <- specializedBase)
        if (originalTraitFlag(clazz))
          clazz.resetFlag(ABSTRACT)
        else
          clazz.resetFlag(ABSTRACT | TRAIT)

      val mbSubst = MiniboxSubst(miniboxedEnv)
      val tree2 = beforeMinibox(d.retyped(
        localTyper.context1.asInstanceOf[d.Context],
        tree,
        source.enclClass,
        symbol.enclClass,
        mbSubst,
        mbSubst.deepSubst
      ))

//      println(tree2)
//      println("\n\n")

      // get back flags
      for (clazz <- specializedBase)
        clazz.setFlag(ABSTRACT | TRAIT)

      tree2
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
      val env = typeEnv.getOrElse(symbol, EmptyTypeEnv)
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

      val symSubstituter = new TreeSymSubstituter(
        parameters.flatten ::: origtparams,
        vparams.map(_.symbol) ::: newtparams)

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
