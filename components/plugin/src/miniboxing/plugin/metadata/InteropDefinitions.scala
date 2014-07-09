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
//
package miniboxing.plugin
package metadata

import scala.tools.nsc.plugins.PluginComponent
import scala.collection.immutable.ListMap

trait InteropDefinitions {
  this: PluginComponent =>

  import global._
  import definitions._

  lazy val mbFunctionClass = {
    // This is what is should look like:
    // ```
    // package __root__.scala {
    // class mbFunction extends Annotation with TypeConstraint
    // }
    // ```
    val AnnotationName = "scala.annotation.Annotation"
    val TypeConstrName = "scala.annotation.TypeConstraint"
    val AnnotationTpe = rootMirror.getRequiredClass(AnnotationName).tpe
    val TypeConstrTpe = rootMirror.getRequiredClass(TypeConstrName).tpe

    val StorageName = newTypeName("mbFunction")
    val StorageSym = ScalaPackageClass.newClassSymbol(StorageName, NoPosition, 0L)
    StorageSym setInfoAndEnter ClassInfoType(List(AnnotationTpe, TypeConstrTpe), newScope, StorageSym)
    StorageSym
  }

  lazy val MiniboxedFunction0Class = global.rootMirror.getRequiredClass("miniboxing.runtime.MiniboxedFunction0")
  lazy val MiniboxedFunction1Class = global.rootMirror.getRequiredClass("miniboxing.runtime.MiniboxedFunction1")
  lazy val MiniboxedFunction2Class = global.rootMirror.getRequiredClass("miniboxing.runtime.MiniboxedFunction2")
  lazy val Function0Class = global.definitions.FunctionClass(0)
  lazy val Function1Class = global.definitions.FunctionClass(1)
  lazy val Function2Class = global.definitions.FunctionClass(2)

  def flag_rewire_functionX: Boolean

  // Addons, not yet separated:

  implicit class RichType(tpe: Type) {
    def isMbFunction: Boolean = tpe.dealiasWiden.annotations.exists(_.tpe.typeSymbol == mbFunctionClass)
    def withMbFunction: Type = tpe.withAnnotations(List(Annotation.apply(mbFunctionClass.tpe, Nil, ListMap.empty)))
    def withoutMbFunction: Type = tpe.filterAnnotations(_.tpe.typeSymbol != mbFunctionClass)
  }

  implicit class RichTree(tree: Tree) {
    def isStorage: Boolean = tree.tpe.isMbFunction
  }
}
