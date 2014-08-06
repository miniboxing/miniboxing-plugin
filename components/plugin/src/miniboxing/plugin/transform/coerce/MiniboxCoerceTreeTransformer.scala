//
//     _____   .__         .__ ___.                    .__ scala-miniboxing.org
//    /     \  |__|  ____  |__|\_ |__    ____  ___  ___|__|  ____     ____
//   /  \ /  \ |  | /    \ |  | | __ \  /  _ \ \  \/  /|  | /    \   / ___\
//  /    Y    \|  ||   |  \|  | | \_\ \(  <_> ) >    < |  ||   |  \ / /_/  >
//  \____|__  /|__||___|  /|__| |___  / \____/ /__/\_ \|__||___|  / \___  /
//          \/          \/          \/               \/         \/ /_____/
// Copyright (c) 2012-2014 Scala Team, École polytechnique fédérale de Lausanne
//
// Authors:
//    * Vlad Ureche
//    * Eugene Burmako
//
package miniboxing.plugin
package transform
package coerce

import scala.tools.nsc._
import scala.tools.nsc.typechecker._
import scala.tools.nsc.transform.TypingTransformers
import scala.util.DynamicVariable
import scala.collection.immutable.ListMap

trait MiniboxCoerceTreeTransformer extends TypingTransformers {
  self: MiniboxCoerceComponent =>

  import minibox._
  import global._

  implicit class RichType(tpe: Type) {
    def getStorageRepr: Symbol = tpe.dealiasWiden.annotations.filter(_.tpe.typeSymbol == StorageClass) match {
      case Nil         => assert(false, "No storage type detected?!?"); ???
      case List(annot) => annot.tpe.typeArgs(0).typeSymbol
    }
    def isStorage: Boolean = tpe.dealiasWiden.annotations.exists(_.tpe.typeSymbol == StorageClass)
    def withStorage(store: Type): Type = tpe.withAnnotations(List(Annotation.apply(appliedType(StorageClass.tpe, List(store)), Nil, ListMap.empty)))
    def withoutStorage: Type = tpe.filterAnnotations(_.tpe.typeSymbol != StorageClass)
  }

  implicit class RichTree(tree: Tree) {
    def isStorage: Boolean = tree.tpe.isStorage
  }

  class CoercePhase(prev: Phase) extends StdPhase(prev) {
    override def name = MiniboxCoerceTreeTransformer.this.phaseName
    override def checkable = false
    def apply(unit: CompilationUnit): Unit = {

      // do this before adapting the tree, so we crash'n'burn
      // if any instantiation wasn't done properly.
      adaptClassFieldsAndCtors()

      val tree = afterMiniboxCoerce(new TreeAdapters().adapt(unit))
      tree.foreach(node => assert(node.tpe != null, node))
    }

    /*
     * This removes fields and constructors from a class while leaving the
     * setters and getters in place. The effect is that the class automatically
     * becomes an interface
     */
    private def adaptClassFieldsAndCtors() = {
      import global.Flag._
      import global.definitions._

      // can be applied as many times as necessary
      for (clazz <- metadata.allStemClasses) {
        val decls = clazz.info.decls
        for (mbr <- decls) {
          if ((mbr.isTerm && !mbr.isMethod) || (mbr.isConstructor))
            decls unlink mbr
        }
      }

      // remove dummy constructors
      for (dummy <- metadata.dummyConstructors)
        dummy.owner.info.decls unlink dummy
    }
  }

  class TreeAdapters extends Analyzer {
    var indent = 0
    lazy val global: self.global.type = self.global

    def adapt(unit: CompilationUnit): Tree = {
      val context = rootContext(unit)
      // turnOffErrorReporting(this)(context)
      val checker = new TreeAdapter(context)
      unit.body = checker.typed(unit.body)
      unit.body
    }

    override def newTyper(context: Context): Typer =
      new TreeAdapter(context)

    def adaptdbg(ind: Int, msg: => String): Unit = {
//       println("  " * ind + msg)
    }

    class TreeAdapter(context0: Context) extends Typer(context0) {
      override protected def finishMethodSynthesis(templ: Template, clazz: Symbol, context: Context): Template =
        templ

      def supertyped(tree: Tree, mode: Mode, pt: Type): Tree =
        super.typed(tree, mode, pt)

      override protected def adapt(tree: Tree, mode: Mode, pt: Type, original: Tree = EmptyTree): Tree = {
        val oldTpe = tree.tpe
        val newTpe = pt
        def superAdapt =
          if (oldTpe <:< newTpe)
            super.adapt(tree, mode, pt, original)
          else
            if (flag_strict_typechecking)
              super.adapt(tree, mode, pt, original)
            else
              tree.setType(newTpe)

        if (tree.isTerm) {
          if ((oldTpe.isStorage ^ newTpe.isStorage) && (!pt.isWildcard)) {
            val conversion = if (oldTpe.isStorage) marker_minibox2box else marker_box2minibox
            val (tpe, repr) =
              if (oldTpe.isStorage)
                (oldTpe.dealiasWiden.withoutStorage, oldTpe.getStorageRepr)
              else
                (newTpe.dealiasWiden.withoutStorage, newTpe.getStorageRepr)
            val tree1 = gen.mkMethodCall(conversion, List(tpe, repr.tpeHK), List(tree))
            val tree2 = super.typed(tree1, mode, pt)
            assert(tree2.tpe != ErrorType, tree2)
            // super.adapt is automatically executed when calling super.typed
            tree2
          } else if (oldTpe.isStorage && (oldTpe.isStorage == newTpe.isStorage) && !(oldTpe <:< newTpe)) {
            val repr1 = oldTpe.getStorageRepr
            val repr2 = newTpe.getStorageRepr
            if (repr1 != repr2) {
              // representation mismatch
              val tree1 = gen.mkMethodCall(marker_minibox2minibox, List(oldTpe.dealiasWiden.withoutStorage, repr1.tpeHK, repr2.tpeHK), List(tree))
              super.typed(tree1, mode, pt)
            } else {
              // workaround the isSubType issue with singleton types
              // and annotated types (see mb_erasure_torture10.scala)
              tree.setType(newTpe)
              tree
            }
          } else
            superAdapt
        } else {
          superAdapt
        }
      }

      case object AlreadyTyped

      override def typed(tree: Tree, mode: Mode, pt: Type): Tree = {
        val ind = indent
        indent += 1
        adaptdbg(ind, " <== " + tree + ": " + showRaw(pt, true, true, false, false) + "  now: " + tree.tpe)
        val res = tree match {
          case EmptyTree | TypeTree() =>
            super.typed(tree, mode, pt)
          case _ if tree.tpe == null =>
            super.typed(tree, mode, pt)

          case Select(qual, meth) if qual.isTerm && tree.symbol.isMethod =>
            val qual2 =
               if (qual.hasAttachment[AlreadyTyped.type])
                 qual
               else {
                 val res = super.typedQualifier(qual.setType(null), mode, WildcardType)
                 res.updateAttachment[AlreadyTyped.type](AlreadyTyped)
                 res
               }

            if (qual2.isStorage) {
              val tpe2 = if (qual2.tpe.hasAnnotation(StorageClass)) qual2.tpe else qual2.tpe.widen
              val tpe3 = tpe2.removeAnnotation(StorageClass)
              //val qual3 = super.typedQualifier(qual.setType(null), mode, tpe3)
              val storageType = qual2.tpe.getStorageRepr.tpeHK
              val qual3 =  gen.mkMethodCall(gen.mkAttributedRef(marker_minibox2box), List(tpe3, storageType), List(qual2))
              super.typed(Select(qual3, meth) setSymbol tree.symbol, mode, pt)
            } else {
              tree.setType(null)
              super.typed(tree, mode, pt)
            }
          case _ =>
            tree.setType(null)
            super.typed(tree, mode, pt)
        }

        // Stupid hack to get rid of an error when typing the <outer>
        // reference - the typer set the Outer.type as type instead of
        // ()Outer.type. There, I fixed it:
        if (tree.hasSymbolField && tree.symbol.name.decoded == "<outer>" && !tree.isInstanceOf[Apply])
          tree.tpe match {
            case MethodType(Nil, _) => // ok
            case _ => tree.setType(MethodType(Nil, tree.tpe))
          }

        adaptdbg(ind, " ==> " + res + ": " + res.tpe)
//        if (res.tpe == ErrorType)
//          adaptdbg(ind, "ERRORS: " + context.errBuffer)
        indent -= 1
        res
      }
    }
  }
}

