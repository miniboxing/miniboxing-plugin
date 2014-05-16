package miniboxing.plugin
package transform
package dupl

import scala.tools.nsc.transform.TypingTransformers
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.typechecker._
import scala.reflect.internal.Flags._

trait MiniboxDuplTreeTransformation extends TypingTransformers {
  self: MiniboxDuplComponent =>

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
//    println("  " * ttindent + " <= type tag trees for " + owner + " owner chain: " + owner.ownerChain.reverse)
    val prev = if (owner.owner != NoSymbol) typeTagTrees(owner.owner) else Map.empty
    val res = prev ++
      inheritedDeferredTypeTags.getOrElse(owner, Map.empty).map({case (method, t) => (t, { gen.mkMethodCall(method, List())})}) ++
      primaryDeferredTypeTags.getOrElse(owner, Map.empty).map({case (method, t) => (t, { gen.mkMethodCall(method, List())})}) ++
      globalTypeTags.getOrElse(owner, Map.empty).map({case (tag, t) => (t, gen.mkAttributedSelect(gen.mkAttributedThis(tag.owner),tag))}) ++
      localTypeTags.getOrElse(owner, Map.empty).map({case (tag, t) => (t, gen.mkAttributedIdent(tag))}) ++
      standardTypeTagTrees // override existing type tags
//    println("  " * ttindent + " => type tag trees for " + owner + ": " + res)
    ttindent -= 1
    res
  }

  def reportError[T](location: Symbol)(body: =>T)(handler: TypeError => T): T =
    try body
    catch {
      case te: TypeError =>
        reporter.error(te.pos, s"[ occured while creating miniboxed ${location.fullLocationString} ]\n${te.msg}")
        //(new Exception()).printStackTrace()
        handler(te)
    }

  /**
   * The tree transformer that adds the trees for the specialized classes inside
   * the current package.
   */
  class MiniboxTreeTransformer(unit: CompilationUnit) extends TreeRewriter(unit) {

    import global._

    /** This duplicator additionally performs casts of expressions if that is allowed by the `casts` map. */
    class Duplicator(casts: Map[Symbol, Type]) extends {
      val global: MiniboxDuplTreeTransformation.this.global.type = MiniboxDuplTreeTransformation.this.global
      val miniboxing: MiniboxDuplComponent { val global: MiniboxDuplTreeTransformation.this.global.type } =
        MiniboxDuplTreeTransformation.this.asInstanceOf[MiniboxDuplComponent { val global: MiniboxDuplTreeTransformation.this.global.type }]
    } with miniboxing.plugin.transform.dupl.Duplicators {
      private val (castfrom, castto) = casts.unzip
      private object CastMap extends SubstTypeMap(castfrom.toList, castto.toList)

      class BodyDuplicator(_context: Context) extends super.BodyDuplicator(_context) {
        override def castType(tree: Tree, pt: Type): Tree = {
          tree.tpe = null
          tree
        }
      }

      protected override def newBodyDuplicator(context: Context) = new BodyDuplicator(context)
    }

    def typeTagTrees(member: Symbol = currentOwner) =
      MiniboxDuplTreeTransformation.this.typeTagTrees(member)

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
      case New(cl)     => afterMiniboxDupl(cl.tpe.typeSymbol.info); cl.tpe
      case This(clazz) => afterMiniboxDupl(currentClass.info); appliedType(tree.symbol, currentClass.typeParams.map(_.tpe): _*)
      case Super(qual, _) => tree.tpe
      case _ => tree.tpe
    }

    def extractFunctionQualifierType(tree: Tree): Type = tree match {
      case Select(newQual, _) => newQual.tpe
      case TypeApply(fun, _) => extractFunctionQualifierType(fun)
      case Apply(fun, _) => extractFunctionQualifierType(fun)
      case _ => NoType
    }

    def rewrite(tree: Tree): Result = {
      curTree = tree

      // make sure specializations have been performed
      tree match {
        case t: SymTree if t.symbol != null => afterMiniboxDupl(t.symbol.info)
        case _ =>
      }

      tree match {

        case ClassDef(_, _, _, impl: Template) if isSpecializableClass(tree.symbol) =>

          // The base trait for the current class
          val baseClassSym = tree.symbol
          val baseTrait = deriveClassDef(tree)(rhs => atOwner(tree.symbol)(transformTemplate(rhs)))

          // The specialized classes for the current class
          val specClassSymbols = specializedClasses(baseClassSym).values.toList.sortBy(_.name.toString)
          val specClasses: List[Tree] =
            for (specClassSym <- specClassSymbols) yield {
              debuglog("Creating specialized class " + specClassSym.defString + " for " + baseClassSym)
              val classDef = atPos(baseClassSym.pos)(classDefTreeFromSym(specClassSym))
              // type check and transform the class before returning the tree
              transform(localTyper.typed(classDef))
            }

          baseTrait :: specClasses

        case Template(parents, self, body) =>
          MethodBodiesCollector(tree)
          afterMiniboxDupl(tree.symbol.enclClass.info)

          //  This is either a class that has nothing to do with miniboxing or that is the base
          //  class (now trait) for the specialization.
          //  Also collect the bodies of the methods that need to be copied and specialized.
          val sym = tree.symbol.enclClass
          val decls = afterMiniboxDupl(sym.info).decls.toList

          // members
          val specMembers = createMethodTrees(tree.symbol.enclClass) map (localTyper.typed)
          val bodyDefs =
            if (specializedBase(tree.symbol.enclClass)) {
              var announce = true
              body.map({
                case dt: DefTree if decls.contains(dt.symbol) =>
                  Some(dt)
                case vd: ValDef if vd.symbol.isValue && !vd.symbol.isMethod =>
                  None
                case dd: DefDef if dd.symbol.isConstructor =>
                  None
                case _: DefTree =>
                  None
                case other if !other.isInstanceOf[DefTree] =>
                  if (announce) {
                    unit.warning(other.pos, "Side-effecting constructor statement will not be specialized " +
                        "in miniboxing annotated class/trait " + tree.symbol.enclClass.name +
                        ". (internal tree: " + other + ")")
                    announce = false
                  }
                  Some(other)
              }).flatten
            } else
              body
          val memberDefs = atOwner(currentOwner)(transformStats(bodyDefs ::: specMembers, sym))

          // parents
          val parents1 = map2(sym.info.parents, parents)((tpe, parent) => TypeTree(tpe) setPos parent.pos)

          // new template def
          val templateDef = treeCopy.Template(tree, parents1, self, atOwner(currentOwner)(memberDefs))
          val tree1 = localTyper.typedPos(tree.pos)(templateDef)

          tree1

        // The trait constructor -- which we leave empty as this is just a simple interface, nothing special about it
        case ddef @ DefDef(mods, name, tparams, vparams :: Nil, tpt, _) if specializedBase(ddef.symbol.enclClass) && ddef.symbol.name != nme.MIXIN_CONSTRUCTOR && !notSpecializable(ddef.symbol.enclClass, ddef.symbol) =>
          localTyper.typed(treeCopy.DefDef(ddef, mods, name, tparams, vparamss = List(vparams), tpt, EmptyTree))

        // A definition with empty body - add a body as prescribed by the `methodSpecializationInfo` data structure.
        case ddef @ DefDef(mods, name, tparams, vparams :: Nil, tpt, _) if hasInfo(ddef) =>
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

            // copy the body of the `original` method
            // we have an rhs, specialize it
            case SpecializedImplementationOf(target) =>
              debuglog("")
              debuglog("Generating specialized implmentation for: " + tree)
              val tree1: Tree = addDefDefBody(ddef, target)
              val tree2: Tree = deriveDefDef(tree1)(rhs => atOwner(tree1.symbol)(transform(rhs)))
              debuglog("before rewiring: " + tree1)
              debuglog("after rewriting: " + tree2)
              localTyper.typed(tree2)

            case _: Interface | _ : DeferredTypeTag =>
              tree

            case DeferredTypeTagImplementation(tparam) =>
              val tagTrees = typeTagTrees()
              val localTParam = tparam.tpe.asSeenFrom(currentClass.info.prefix, currentClass).typeSymbol
              localTyper.typed(deriveDefDef(tree)(_ => localTyper.typed(tagTrees(localTParam))))

            case info =>
              sys.error("Unknown info type: " + info)
          }
          res

        case vdef @ ValDef(mods, name, tpt, EmptyTree) if hasInfo(vdef) =>
          memberSpecializationInfo(tree.symbol) match {
            case SpecializedImplementationOf(original) =>
              val newTree = addValDefBody(tree, original)
              localTyper.typed(deriveValDef(newTree)(rhs => atOwner(newTree.symbol)(transform(rhs))))
            case info =>
              sys.error("Unknown info type: " + info)
          }

        // ???
        case DefDef(mods, name, tparams, vparamss, tpt, body) if (tree.symbol.isConstructor &&
          tree.symbol.paramss.head.size != vparamss.head.size) =>
          debug(" => overriding constructor in " + tree.symbol.ownerChain.reverse.map(_.nameString).mkString(".") + ":\n" + tree)
          val result = localTyper.typedPos(tree.pos)(DefDef(tree.symbol, _ => atOwner(tree.symbol)(transform(body))))
          debug(" <= " + result)
          result

        // Error on accessing non-existing fields
        case sel@Select(ths, field) if (ths.symbol ne null) && (ths.symbol != NoSymbol) && { afterMiniboxDupl(ths.symbol.info); specializedBase(ths.symbol) && (sel.symbol.isValue && !sel.symbol.isMethod) } =>
          unit.error(sel.pos, "The program is accessing field " + sel.symbol.name + " of miniboxed class (or trait) " + ths.symbol.name + ", a pattern which becomes invalid after the miniboxing transformation. Please allow Scala to generate getters (and possibly setters) by using val (or var) without the \"private[this]\" qualifier: " + (if (sel.symbol.isMutable) "var " else "val ") + sel.symbol.name + ": " + sel.symbol.info + "\".")
          localTyper.typed(gen.mkAttributedRef(Predef_???))

        // rewiring new class construction
        // new C[Int] => new C_J[Int]
        case New(cl) if specializedClasses.isDefinedAt(cl.tpe.typeSymbol) =>
          val newType = miniboxQualifier(tree.pos, cl.tpe)
          localTyper.typedOperator(New(TypeTree(newType)))

        // rewiring this calls
        // C.this.foo => C_J.this.foo
        case This(cl) if specializedClasses.isDefinedAt(tree.symbol) && !currentOwner.ownerChain.contains(tree.symbol)=>
          val newType = miniboxQualifier(tree.pos, tree.tpe)
          val res = localTyper.typed(This(newType.typeSymbol))
//          println(tree + " ==> " + res)
          res

        // rewire member selection
        case Select(oldQual, mbr) if extractQualifierType(oldQual).typeSymbol.hasFlag(MINIBOXED) || oldQual.isInstanceOf[Super] || overloads.isDefinedAt(tree.symbol) =>
          val oldMbrSym = tree.symbol
          val oldQualTpe: Type = extractQualifierType(oldQual)
          val oldQualSym = tree.symbol.owner //oldQualTpe.typeSymbol
          val (newQual: Tree, newQualTpe: Type, newQualSym: Symbol) =
            oldQual match {
              case sup @ Super(ths, _) =>
                // rewriting member selection on the fly, otherwise this won't work :(
                val selTree = localTyper.typedOperator(Select(Super(transform(ths), sup.mix), mbr))
                val Select(supTree, _) = selTree
                val ntpe = supTree.tpe baseType selTree.symbol.owner
                (supTree, ntpe, selTree.symbol.owner)
              case _ =>
                val nqual = transform(oldQual)
                val ntpe = extractQualifierType(nqual)
                (nqual, ntpe, ntpe.typeSymbol)
            }
          val spec = extractSpec(oldQual.tpe, currentMethod, currentClass)

          // patching for the corresponding symbol in the new receiver
          // new C[Int].foo => new C_J[Int].foo
          // note: constructors are updated in the next step, as their class overloads are recorded
          val newMbrSym =
            if ((oldQualSym != newQualSym) &&
                (baseClass.getOrElse(newQualSym, NoSymbol) == oldQualSym) &&
                (oldMbrSym.name != nme.CONSTRUCTOR)) {
              val updSym = newQualSym.tpe.member(oldMbrSym.name).filter(_.allOverriddenSymbols contains oldMbrSym)
              assert(!updSym.isOverloaded && updSym != NoSymbol)
              updSym
            } else
              oldMbrSym

          // patching according to the receiver's specialization
          // new C[Int].foo_J => new C_J[Int].foo_J
          val specMbrSym = {
            val tpe1 = newQualTpe baseType (newMbrSym.owner)
            extractSpec(tpe1, currentMethod, currentClass) match { // Get the partial specialization
              case Some(pspec) if overloads.get(newMbrSym).flatMap(_.get(pspec)).isDefined =>
//                println()
//                println("spec: " + pspec)
                val newMethodSym = overloads(newMbrSym)(pspec)
                newMethodSym
//              case Some(pspec) =>
//                println()
//                println(!PartialSpec.isAllAnyRef(pspec))
//                println(newMbrSym.defString)
//                println(pspec)
//                println(overloads.get(newMbrSym))
//                println(overloads.get(newMbrSym).flatMap(_.get(pspec)))
//                println(pspec)
//                newMbrSym
              case _ =>
//                println()
//                println("none")
                newMbrSym
            }
          }

          //
          val ntree = localTyper.typedOperator(gen.mkAttributedSelect(newQual, specMbrSym))
//          println()
//          println("initial tree: " + tree + " : " + tree.tpe)
//          println(s"$oldQual ($oldQualSym) vs $newQual ($newQualSym)")
//          println("rewiring original: " + oldMbrSym.defString + " (onwer: " + oldMbrSym.owner + ")")
//          println("rewiring step 1:   " + newMbrSym.defString + " (onwer: " + newMbrSym.owner + ")")
//          println("rewiring step 2:   " + specMbrSym.defString + " (onwer: " + specMbrSym.owner + ")")
//          println(ntree.tpe)

          assert(!dummyConstructors(ntree.symbol), "dummy constructor: " + ntree.symbol.defString + " left in tree " + tree)
          ntree

        case tapply @ TypeApply(oldFun @ Select(qual, fn), targs) =>
          afterMiniboxDupl(oldFun.symbol.owner.info)
          val oldSym = oldFun.symbol
          val oldType = tapply.tpe
          val newFun = transform(oldFun)
          val Select(newQual, _) = newFun
          val newSym = newFun.symbol

          // find the normalized member
          val normSym =
            extractNormSpec(targs.map(_.tpe), newFun.symbol, currentMethod, currentClass) match {
              case Some(newSym) => newSym
              case None => newFun.symbol
            }

          // replace the member by the normalized member
          val ntargs = targs.map(transform)
          val tree1 = if (normSym != newSym)
            TypeApply(Select(newQual, normSym), ntargs)
          else
            TypeApply(newFun, ntargs)

//          println()
//          println("initial tree: " + tree + " : " + tree.tpe)
//          println(s"$oldSym (${oldSym.defString}) vs $newSym (${newSym.defString})")
//          println("rewiring original: " + oldSym.defString + " (onwer: " + oldSym.owner + ")")
//          println("rewiring step 1:   " + newSym.defString + " (onwer: " + newSym.owner + ")")
//          println("rewiring step 2:   " + normSym.defString + " (onwer: " + normSym.owner + ")")
//          println("res: " + tree1)
          val tree2 = localTyper.typedOperator(tree1)
//          println("     : " + tree2.tpe)
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
          val (tagSyms, argTypes) = separateTypeTagArgsInType(newMethodSym.info, targs.map(_.tpe))
          val tagsToTparams1 = localTypeTags.getOrElse(newMethodSym, Map()).toMap
          val tparamInsts = for (tagSym <- tagSyms) yield {
            try {
              val tparam = tagsToTparams1(tagSym)
              val instOwner = extractFunctionQualifierType(newFun).baseType(tparam.owner)
//              println()
//              println(nTewFun)
//              println(extractFunctionQualifierType(newFun))
//              println(instOwner + "  " + instOwner.typeSymbol)
              val tparamFromQualToInst = (instOwner.typeSymbol.typeParams zip instOwner.typeArgs).toMap
              if (targs != null) assert(newMethodSym.info.typeParams.length == targs.length, "Type parameter mismatch in rewiring from " + oldMethodSym.defString + " to " + newMethodSym.defString + ": " + targs)
              val tparamFromNormToInst = if (targs != null) (newMethodSym.info.typeParams zip targs.map(_.tpe)).toMap else Map.empty
              val tparamToInst = tparamFromQualToInst ++ tparamFromNormToInst ++ ScalaValueClasses.map(sym => (sym, sym.tpe))
//              println(tparamFromQualToInst)
//              println(tparamToInst)
              tparam.map(tparamToInst(_).typeSymbol)
            } catch {
              case ex: Exception =>
                println("Tag not found:")
                println(newMethodSym.defString)
                println(tagSyms)
                println(argTypes)
                println(localTypeTags.getOrElse(newMethodSym, Map()))
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
          val localTagArgs = tparamInsts.map(typeTags).map(localTyper.typed(_))

          val tree1 = gen.mkMethodCall(newFun, localTagArgs ::: args)
//          println()
//          println(tree + " ==> " + tree1)
//          println(tree1)
//          println(tree1.tpe)
//          println(newFun)
//          println(newFun.symbol.defString)
//          println(newFun.tpe)
//          println(tree1)
          val tree2 = localTyper.typed(tree1)
//          println("after")
          tree2

        case _ =>
          Descend
      }
    }

    def extractNormSpec(targs: List[Type], target: Symbol, inMethod: Symbol, inClass: Symbol): Option[Symbol] = {
      val pSpecFromBaseClass = partialSpec.getOrElse(inClass, Map.empty)
      val mapTpar = typeEnv.getOrElse(inClass, Map.empty)
      val pSpecInCurrentClass = pSpecFromBaseClass.map({ case (tp, status) => (mapTpar.getOrElse(tp, tp.tpe).typeSymbol, status)})
      val pSpecInCurrentMethod = inMethod.ownerChain.filter(_.isMethod).flatMap(normalSpec.getOrElse(_, Map.empty))
      val pSpec = pSpecInCurrentClass ++ pSpecInCurrentMethod

      if (normbase.isDefinedAt(target)) {
        val tparams = afterMiniboxDupl(target.info).typeParams
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
//        println(!notSpecializable(target.owner, target))
//        println(target.typeParams.exists(isSpecialized(target.owner, _)))
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

//      println(showRaw(qualTpe) + " in " + inClass + " and " + inMethod)

      qualTpe match {
        case ThisType(cls) if baseClass.isDefinedAt(inClass) && inClass == cls =>
          // we're in the interface
          None
        case ThisType(cls) if baseClass.isDefinedAt(inClass) && baseClass(inClass) == cls =>
          Some(pSpecFromBaseClass)
//      since we don't specialize nested classes, this case will never occur:
//        case t: ThisType =>
//          extractSpec(t.widen, inMethod, inClass)
        case SingleType(pre, x) =>
          extractSpec(qualTpe.widen, inMethod, inClass)
        case PolyType(tparams, rest) =>
          extractSpec(rest, inMethod, inClass)
        case TypeRef(pre, clazz, args) =>
          import miniboxing.runtime.MiniboxConstants._
          val tparams = afterMiniboxDupl(baseClass.getOrElse(clazz, clazz).info).typeParams
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

    // We collect the bodies of the target methods in order to have them available
    // for copying inside the methods that are specialized implementations of them.
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

      def getMethodBody(meth: Symbol) =
        try {
          body.apply(meth)
        } catch {
          case e: Throwable =>
            unit.error(meth.pos, "Internal miniboxing plugin error: Method body for " + meth + " not collected.")
            (localTyper.typed(gen.mkMethodCall(Predef_???, Nil)), meth.paramss)
        }
      def getFieldBody(fld: Symbol) = body(fld)._1
    }

    private def duplicateBody(tree0: Tree, source: Symbol, castmap: TypeEnv = Map.empty): Tree = {

      val symbol = tree0.symbol
      val miniboxedEnv = typeEnv.getOrElse(symbol, EmptyTypeEnv)
      val miniboxedTypeTags = typeTagTrees(symbol)

      debug(s"duplicating tree: for ${symbol} based on ${source}:\n${tree0}")

//      println("DUPLICATING + " + symbol.defString + " based on " + source.defString)
//      println(miniboxedEnv)
//      println(tree)

      //val access_error = !(new AccessibiltyChecker(unit, symbol.owner)).apply(tree0)
      val tree = tree0 //if (access_error)  else tree0

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
      val tree2 =
        reportError(symbol)(
          beforeMiniboxDupl(d.retyped(
            localTyper.context1.asInstanceOf[d.Context],
            tree,
            source.enclClass,
            symbol.enclClass,
            mbSubst,
            mbSubst.deepSubst
          ))
        )(_ => localTyper.typed(deriveDefDef(tree)(_ => localTyper.typed(gen.mkMethodCall(Predef_???, Nil)))))

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
    private def addDefDefBody(tree: DefDef, source: Symbol): Tree = {
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

      val meth = copyDefDef(tree)(rhs = newBody)
      duplicateBody(meth, source)
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
