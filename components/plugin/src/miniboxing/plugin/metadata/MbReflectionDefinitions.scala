//
//     _____   .__         .__ ____.                     .__ scala-miniboxing.org
//    /_   _\  |__|  ____  |__|\_  |__    _____  ___  ___|__|  ____    _____
//   / o\ /o \ |  | /    \ |  | |  __ \  /  ___\ \  \/  /|  | /    \  /  ___\
//  /    Y    \|  ||   |  \|  | |  \_\ \(  (_)  ) >    < |  ||   |  \(  /_/  )
//  \____|__  /|__||___|  /|__| |____  / \_____/ /__/\_ \|__||___|  / \___  /
//          \/          \/           \/                \/         \/ /_____/
// Copyright (c) 2011-2015 Scala Team, École polytechnique fédérale de Lausanne
//
// Authors:
//    * Vlad Ureche
//    * Nicolas Stucki
//
// Thanks to: @tixxit (Tom Switzer), @dlwh (David Hall) and @ichoran (Rex Kerr)
// for their very good feedback!
//
package miniboxing.plugin
package metadata

import scala.tools.nsc.plugins.PluginComponent

trait MbReflectionDefinitions {
  this: MiniboxInjectComponent =>

  import global._
  import definitions._
  import miniboxing.runtime.MiniboxConstants._

  // user-facing methods:
  lazy val MiniboxingReflection     = rootMirror.getRequiredModule("scala.MbReflection")
  lazy val reifiedType              = definitions.getMember(MiniboxingReflection, newTermName("reifiedType"))
  lazy val storageType              = definitions.getMember(MiniboxingReflection, newTermName("storageType"))
  lazy val isMiniboxed              = definitions.getMember(MiniboxingReflection, newTermName("isMiniboxed"))
  lazy val reflectionMethods        = Set(reifiedType, storageType, isMiniboxed)

  // implementations:
  lazy val MiniboxingReflectionImpl = rootMirror.getRequiredModule("miniboxing.runtime.reflect.MbReflectionImpl")
  lazy val reifiedTypeImpl          = definitions.getMember(MiniboxingReflectionImpl, newTermName("reifiedTypeImpl"))
  lazy val storageTypeImpl          = definitions.getMember(MiniboxingReflectionImpl, newTermName("storageTypeImpl"))



  object MiniboxingReflectionMethod {

    // returns a function that, based on the current owner, rewrites the tree
    def unapply(tree: Tree): Option[Symbol => Tree] = {
      tree match {
        case Apply(TypeApply(mth, List(tpt)), Nil) if reflectionMethods(mth.symbol) =>
          Some(
            (currentOwner: Symbol) => {
              if (!flag_two_way)
                global.reporter.error(tree.pos,
                    "The miniboxing reflection can only be used with the 2-way miniboxing transformation (without " +
                    "the `-P:minibox:Yone-way` argument).")

              val (mbox, tag) = specialization(tree, tpt, currentOwner)
              mth.symbol match {
                case `isMiniboxed` =>
                  Literal(Constant(mbox))
                case `reifiedType` =>
                  gen.mkMethodCall(reifiedTypeImpl, List(tag))
                case `storageType` =>
                  gen.mkMethodCall(storageTypeImpl, List(tag))
              }
            }
          )
        case _ =>
          None
      }
    }

    // extract whether a type parameter is specialized and its tag
    private def specialization(tree: Tree, tpt: Tree, owner: Symbol): (Boolean, Tree) = {
      lazy val unknownTag = gen.mkMethodCall(Predef_???, Nil)
      lazy val referenceTag = Literal(Constant(REFERENCE))

      tpt.tpe match {
        case tpe if tpe.typeSymbol.deSkolemize.isTypeParameter || ScalaValueClasses.contains(tpe.typeSymbol) =>
          val sym = tpe.typeSymbol.deSkolemize
          val specs = PartialSpec.specializationsFromOwnerChain(owner)
          specs.collect({ case (`sym`, spec) => spec }) match {
            case List(Miniboxed(_)) =>
              typeTagTrees(owner).get(sym) match {
                case Some(tagTree) =>
                  (true, tagTree)
                case _ =>
                  global.reporter.error(tree.pos,
                      "The miniboxing reflection encountered an internal error: missing tag for miniboxed " +
                      "type parameter:\nspecs:" + specs + "\ntags:" + typeTagTrees(owner) + "\nPlease report " +
                      "this bug to https://github.com/miniboxing/miniboxing-plugin/issues."
                      )
                  (true, unknownTag)
              }
            case List(Boxed) =>
              (false, referenceTag)
            case Nil if ScalaValueClasses.contains(tpe.typeSymbol) =>
              (true, standardTypeTagTrees(tpe.typeSymbol))
            case Nil =>
              global.reporter.warning(tree.pos,
                  "The miniboxing reflection was requested for non-miniboxed type parameter " + sym.name + ". While " +
                  "this is correct, the result will always be negative."
              )
              (false, referenceTag)
            case _ =>
              global.reporter.error(tree.pos,
                  "The miniboxing reflection encountered an internal error: no specialization for miniboxed " +
                  "type parameter:\nspecs:" + specs + "\ntags:" + typeTagTrees(owner) + "\nPlease report " +
                  "this bug to https://github.com/miniboxing/miniboxing-plugin/issues."
                  )
              (false, unknownTag)
          }
        case _ =>
          global.reporter.error(tree.pos,
              "The miniboxing reflection can only be called with a type parameter or a primitive type argument.")
          (false, unknownTag)
      }
    }
  }
}